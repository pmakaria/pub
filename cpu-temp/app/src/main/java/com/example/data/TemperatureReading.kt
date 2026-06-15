package com.example.data

import androidx.room.Entity
import androidx.room.PrimaryKey

@Entity(tableName = "temperature_readings")
data class TemperatureReading(
    @PrimaryKey(autoGenerate = true) val id: Long = 0L,
    val temperature: Float,
    val timestamp: Long = System.currentTimeMillis(),
    val source: String
)
