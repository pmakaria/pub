package com.example.data

import kotlinx.coroutines.flow.Flow

class TemperatureRepository(private val temperatureDao: TemperatureDao) {

    fun getReadingsPast24Hours(sensorType: String, timeThreshold: Long): Flow<List<TemperatureReading>> {
        return temperatureDao.getReadingsPast24Hours(sensorType, timeThreshold)
    }

    suspend fun insertReading(reading: TemperatureReading) {
        temperatureDao.insertReading(reading)
    }

    suspend fun pruneOldReadings(timeThreshold: Long) {
        temperatureDao.pruneOldReadings(timeThreshold)
    }

    suspend fun clearHistory() {
        temperatureDao.clearAllReadings()
    }
}
