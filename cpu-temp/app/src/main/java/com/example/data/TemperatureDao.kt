package com.example.data

import androidx.room.*
import kotlinx.coroutines.flow.Flow

@Dao
interface TemperatureDao {
    @Query("SELECT * FROM temperature_readings ORDER BY timestamp DESC")
    fun getAllReadings(): Flow<List<TemperatureReading>>

    @Query("SELECT * FROM temperature_readings WHERE sensorType = :sensorType AND timestamp >= :thresholdTime ORDER BY timestamp DESC")
    fun getReadingsPast24Hours(sensorType: String, thresholdTime: Long): Flow<List<TemperatureReading>>

    @Insert(onConflict = OnConflictStrategy.REPLACE)
    suspend fun insertReading(reading: TemperatureReading)

    @Query("DELETE FROM temperature_readings WHERE timestamp < :thresholdTime")
    suspend fun pruneOldReadings(thresholdTime: Long)

    @Query("DELETE FROM temperature_readings")
    suspend fun clearAllReadings()
}
