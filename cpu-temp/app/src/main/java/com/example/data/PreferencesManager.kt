package com.example.data

import android.content.Context
import android.content.SharedPreferences

class PreferencesManager(context: Context) {
    private val prefs: SharedPreferences = context.getSharedPreferences("cpu_temp_prefs", Context.MODE_PRIVATE)

    companion object {
        const val KEY_UPDATE_FREQUENCY = "update_frequency"
        const val KEY_DEMO_MODE = "demo_mode"
        const val KEY_THEME = "ui_theme" // "system", "dark", "light"
        const val KEY_TEMP_UNIT = "temp_unit" // "celsius", "fahrenheit"
        const val KEY_WARNING_THRESHOLD = "warning_threshold" // Float: target hot temp limit (e.g. 42.0)

        const val DEFAULT_FREQUENCY = 5 // 5 seconds
        const val DEFAULT_WARNING_THRESHOLD = 42.0f
    }

    var updateFrequency: Int
        get() = prefs.getInt(KEY_UPDATE_FREQUENCY, DEFAULT_FREQUENCY)
        set(value) = prefs.edit().putInt(KEY_UPDATE_FREQUENCY, value).apply()

    var demoModeEnabled: Boolean
        get() = prefs.getBoolean(KEY_DEMO_MODE, false)
        set(value) = prefs.edit().putBoolean(KEY_DEMO_MODE, value).apply()

    var themeSetting: String
        get() = prefs.getString(KEY_THEME, "system") ?: "system"
        set(value) = prefs.edit().putString(KEY_THEME, value).apply()

    var tempUnit: String
        get() = prefs.getString(KEY_TEMP_UNIT, "celsius") ?: "celsius"
        set(value) = prefs.edit().putString(KEY_TEMP_UNIT, value).apply()

    var warningThreshold: Float
        get() = prefs.getFloat(KEY_WARNING_THRESHOLD, DEFAULT_WARNING_THRESHOLD)
        set(value) = prefs.edit().putFloat(KEY_WARNING_THRESHOLD, value).apply()
}
