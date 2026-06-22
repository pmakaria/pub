package com.example.ui

import android.app.Application
import androidx.lifecycle.AndroidViewModel
import androidx.lifecycle.viewModelScope
import com.example.data.*
import com.example.util.CpuTempReader
import com.example.util.TempResult
import kotlinx.coroutines.ExperimentalCoroutinesApi
import kotlinx.coroutines.Job
import kotlinx.coroutines.delay
import kotlinx.coroutines.flow.*
import kotlinx.coroutines.launch

@OptIn(ExperimentalCoroutinesApi::class)
class CpuTempViewModel(application: Application) : AndroidViewModel(application) {
    private val db = AppDatabase.getDatabase(application)
    private val repository = TemperatureRepository(db.temperatureDao())
    private val prefsManager = PreferencesManager(application)

    // Track active settings as state
    private val _updateFrequency = MutableStateFlow(prefsManager.updateFrequency)
    val updateFrequency = _updateFrequency.asStateFlow()

    private val _demoModeEnabled = MutableStateFlow(prefsManager.demoModeEnabled)
    val demoModeEnabled = _demoModeEnabled.asStateFlow()

    private val _themeSetting = MutableStateFlow(prefsManager.themeSetting)
    val themeSetting = _themeSetting.asStateFlow()

    private val _tempUnit = MutableStateFlow(prefsManager.tempUnit)
    val tempUnit = _tempUnit.asStateFlow()

    private val _warningThreshold = MutableStateFlow(prefsManager.warningThreshold)
    val warningThreshold = _warningThreshold.asStateFlow()

    private val _sensorType = MutableStateFlow(prefsManager.sensorType)
    val sensorType = _sensorType.asStateFlow()

    // Active physical reading
    private val _currentReading = MutableStateFlow<TempResult?>(null)
    val currentReading = _currentReading.asStateFlow()

    // Poll Job
    private var pollJob: Job? = null

    // Past 24 hours readings from DB
    val readingsPast24Hours: StateFlow<List<TemperatureReading>> = combine(
        flow {
            while (true) {
                val threshold = System.currentTimeMillis() - 24 * 60 * 60 * 1000L
                emit(threshold)
                // Recalculate 24 hours time threshold every 10 seconds to slide the window forward
                delay(10000)
            }
        },
        _sensorType
    ) { threshold, type ->
        threshold to type
    }.flatMapLatest { (threshold, type) ->
        repository.getReadingsPast24Hours(type, threshold)
    }.stateIn(
        scope = viewModelScope,
        started = SharingStarted.WhileSubscribed(5000),
        initialValue = emptyList()
    )

    // Calculate dynamic min and max temperature from the past 24 hours readings
    val minTempPast24Hours: StateFlow<Float?> = readingsPast24Hours.map { readings ->
        readings.minOfOrNull { it.temperature }
    }.stateIn(viewModelScope, SharingStarted.WhileSubscribed(5000), null)

    val maxTempPast24Hours: StateFlow<Float?> = readingsPast24Hours.map { readings ->
        readings.maxOfOrNull { it.temperature }
    }.stateIn(viewModelScope, SharingStarted.WhileSubscribed(5000), null)

    init {
        startPolling()
    }

    fun startPolling() {
        pollJob?.cancel()
        pollJob = viewModelScope.launch {
            while (true) {
                triggerReading()
                // Dynamic wait based on active update interval (seconds to ms)
                delay(_updateFrequency.value * 1000L)
            }
        }
    }

    private suspend fun triggerReading() {
        val context = getApplication<Application>().applicationContext
        val result = if (_sensorType.value == "battery") {
            CpuTempReader.readBatteryTemperature(context, _demoModeEnabled.value)
        } else {
            CpuTempReader.readCpuTemperature(context, _demoModeEnabled.value)
        }
        _currentReading.value = result

        // Save to Room database
        repository.insertReading(
            TemperatureReading(
                temperature = result.temp,
                source = result.source,
                sensorType = _sensorType.value
            )
        )

        // Prune any records older than 24 hours to keep the data set compact
        val threshold = System.currentTimeMillis() - 24 * 60 * 60 * 1000L
        repository.pruneOldReadings(threshold)
    }

    fun setUpdateFrequency(frequencySeconds: Int) {
        prefsManager.updateFrequency = frequencySeconds
        _updateFrequency.value = frequencySeconds
        startPolling() // Dynamic reset: reschedule polling schedule immediately
    }

    fun setDemoModeEnabled(enabled: Boolean) {
        prefsManager.demoModeEnabled = enabled
        _demoModeEnabled.value = enabled
        // Trigger immediate check to reflect changes
        viewModelScope.launch {
            triggerReading()
        }
    }

    fun setSensorType(type: String) {
        prefsManager.sensorType = type
        _sensorType.value = type
        // Immediately trigger a reading to update the dashboard instantly with correct sensor info
        viewModelScope.launch {
            triggerReading()
        }
    }

    fun setThemeSetting(theme: String) {
        prefsManager.themeSetting = theme
        _themeSetting.value = theme
    }

    fun setTempUnit(unit: String) {
        prefsManager.tempUnit = unit
        _tempUnit.value = unit
    }

    fun setWarningThreshold(threshold: Float) {
        prefsManager.warningThreshold = threshold
        _warningThreshold.value = threshold
    }

    fun clearHistoryLogs() {
        viewModelScope.launch {
            repository.clearHistory()
            // Force re-trigger of reading so screen has an active point
            triggerReading()
        }
    }
}
