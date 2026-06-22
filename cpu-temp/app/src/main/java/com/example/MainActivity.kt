package com.example

import android.os.Bundle
import androidx.activity.ComponentActivity
import androidx.activity.compose.setContent
import androidx.activity.enableEdgeToEdge
import androidx.compose.animation.*
import androidx.compose.animation.core.*
import androidx.compose.foundation.*
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.lazy.LazyColumn
import androidx.compose.foundation.lazy.items
import androidx.compose.foundation.shape.CircleShape
import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.material.icons.Icons
import androidx.compose.material.icons.filled.*
import androidx.compose.material3.*
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.draw.clip
import androidx.compose.ui.geometry.Offset
import androidx.compose.ui.geometry.Size
import androidx.compose.ui.graphics.Brush
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.graphics.StrokeCap
import androidx.compose.ui.graphics.drawscope.Stroke
import androidx.compose.ui.graphics.nativeCanvas
import androidx.compose.ui.graphics.toArgb
import androidx.compose.ui.platform.LocalContext
import androidx.compose.ui.platform.testTag
import androidx.compose.ui.text.font.FontFamily
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.text.style.TextAlign
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import androidx.lifecycle.compose.collectAsStateWithLifecycle
import androidx.lifecycle.viewmodel.compose.viewModel
import com.example.data.TemperatureReading
import com.example.ui.CpuTempViewModel
import com.example.ui.theme.MyApplicationTheme
import com.example.util.TempResult
import java.text.SimpleDateFormat
import java.util.*

class MainActivity : ComponentActivity() {
    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        enableEdgeToEdge()
        setContent {
            val viewModel: CpuTempViewModel = viewModel()
            val themeSetting by viewModel.themeSetting.collectAsStateWithLifecycle()
            
            // Check dynamic system dark mode and user overrides
            val isDarkTheme = when (themeSetting) {
                "dark" -> true
                "light" -> false
                else -> isSystemInDarkTheme()
            }

            MyApplicationTheme(darkTheme = isDarkTheme) {
                Scaffold(
                    modifier = Modifier
                        .fillMaxSize()
                        .testTag("main_scaffold")
                ) { innerPadding ->
                    CpuTempDashboardScreen(
                        viewModel = viewModel,
                        modifier = Modifier.padding(innerPadding),
                        isDark = isDarkTheme
                    )
                }
            }
        }
    }
}

@Composable
fun CpuTempDashboardScreen(
    viewModel: CpuTempViewModel,
    modifier: Modifier = Modifier,
    isDark: Boolean
) {
    val currentReading by viewModel.currentReading.collectAsStateWithLifecycle()
    val readings by viewModel.readingsPast24Hours.collectAsStateWithLifecycle()
    val minTemp by viewModel.minTempPast24Hours.collectAsStateWithLifecycle()
    val maxTemp by viewModel.maxTempPast24Hours.collectAsStateWithLifecycle()

    val updateFrequency by viewModel.updateFrequency.collectAsStateWithLifecycle()
    val demoMode by viewModel.demoModeEnabled.collectAsStateWithLifecycle()
    val warningThreshold by viewModel.warningThreshold.collectAsStateWithLifecycle()
    val tempUnit by viewModel.tempUnit.collectAsStateWithLifecycle()
    val themeSetting by viewModel.themeSetting.collectAsStateWithLifecycle()
    val sensorType by viewModel.sensorType.collectAsStateWithLifecycle()

    var showSettings by remember { mutableStateOf(false) }
    var showInfoDialog by remember { mutableStateOf(false) }
    var selectedTab by remember { mutableIntStateOf(0) } // 0: Widget, 1: Log, 2: Stats

    // --- Professional Polish Design Theme Color Tokens ---
    val mainBg = if (isDark) Color(0xFF1C1B1F) else Color(0xFFFAF8FD)
    val cardBg = if (isDark) Color(0xFF2B2930) else Color(0xFFF1EEFA)
    val primaryText = if (isDark) Color.White else Color(0xFF1C1B1F)
    val secondaryText = if (isDark) Color(0xFFCAC4D0) else Color(0xFF49454F)
    val brandPrimary = if (isDark) Color(0xFFD0BCFF) else Color(0xFF6750A4)
    val textOnBrand = if (isDark) Color(0xFF381E72) else Color.White
    val navBg = if (isDark) Color(0xFF211F26) else Color(0xFFE5DFF3)
    val dividerColor = if (isDark) Color(0xFF49454F) else Color(0xFFE0DCF0)
    val textColor = primaryText

    val activeTemp = currentReading?.temp ?: 36.5f
    val accentColor = when {
        activeTemp >= warningThreshold -> Color(0xFFFFB4AB) // Warning Coral Pink
        activeTemp >= warningThreshold - 4.0f -> Color(0xFFFFCC00) // High Warning Amber Gold
        else -> brandPrimary
    }

    Box(
        modifier = modifier
            .fillMaxSize()
            .background(mainBg)
    ) {
        Column(
            modifier = Modifier
                .fillMaxSize()
                .navigationBarsPadding()
        ) {
            // ======== 1. CUSTOM SYSTEM TOP HEADER ========
            Row(
                modifier = Modifier
                    .fillMaxWidth()
                    .padding(horizontal = 16.dp, vertical = 12.dp)
                    .height(64.dp),
                verticalAlignment = Alignment.CenterVertically,
                horizontalArrangement = Arrangement.SpaceBetween
            ) {
                Row(
                    verticalAlignment = Alignment.CenterVertically,
                    horizontalArrangement = Arrangement.spacedBy(12.dp)
                ) {
                    DeviceSensorIcon(sensorType = sensorType, tint = brandPrimary, modifier = Modifier.size(32.dp))
                    Column {
                        Text(
                            text = "Device Temp Monitor",
                            fontSize = 20.sp,
                            fontWeight = FontWeight.Bold,
                            color = primaryText
                        )
                        Row(
                            verticalAlignment = Alignment.CenterVertically,
                            horizontalArrangement = Arrangement.spacedBy(4.dp)
                        ) {
                            // Pulsing core sensor dot
                            val infiniteTransition = rememberInfiniteTransition(label = "pulse")
                            val pulseScale by infiniteTransition.animateFloat(
                                initialValue = 0.7f,
                                targetValue = 1.3f,
                                animationSpec = infiniteRepeatable(
                                    animation = tween(1400, easing = LinearEasing),
                                    repeatMode = RepeatMode.Reverse
                                ),
                                label = "scale"
                            )
                            Box(contentAlignment = Alignment.Center, modifier = Modifier.size(10.dp)) {
                                Box(
                                    modifier = Modifier
                                        .size(10.dp * pulseScale)
                                        .clip(CircleShape)
                                        .background(accentColor.copy(alpha = 0.4f))
                                )
                                Box(
                                    modifier = Modifier
                                        .size(6.dp)
                                        .clip(CircleShape)
                                        .background(accentColor)
                                )
                            }
                            Text(
                                text = if (demoMode) "DEMO EMULATOR MODE" else "SYSTEM HARDWARE SENSOR",
                                fontSize = 11.sp,
                                fontWeight = FontWeight.Bold,
                                color = brandPrimary
                            )
                        }
                    }
                }

                Row(horizontalArrangement = Arrangement.spacedBy(8.dp)) {
                    IconButton(
                        onClick = { showInfoDialog = true },
                        modifier = Modifier
                            .size(48.dp)
                            .background(
                                color = if (isDark) Color.White.copy(alpha = 0.06f) else Color.Black.copy(alpha = 0.04f),
                                shape = CircleShape
                            )
                    ) {
                        Icon(
                            imageVector = Icons.Default.Info,
                            contentDescription = "About Information",
                            tint = secondaryText
                        )
                    }

                    IconButton(
                        onClick = { showSettings = !showSettings },
                        modifier = Modifier
                            .size(48.dp)
                            .background(
                                color = if (showSettings) {
                                    brandPrimary.copy(alpha = 0.15f)
                                } else {
                                    if (isDark) Color.White.copy(alpha = 0.06f) else Color.Black.copy(alpha = 0.04f)
                                },
                                shape = CircleShape
                            )
                    ) {
                        Icon(
                            imageVector = Icons.Default.Settings,
                            contentDescription = "System Settings",
                            tint = if (showSettings) brandPrimary else secondaryText
                        )
                    }
                }
            }

            // Expanded Settings Segment (Floating layout overlays)
            AnimatedVisibility(
                visible = showSettings,
                enter = expandVertically() + fadeIn(),
                exit = shrinkVertically() + fadeOut()
            ) {
                Card(
                    modifier = Modifier
                        .fillMaxWidth()
                        .padding(horizontal = 16.dp, vertical = 4.dp)
                        .border(
                            width = 1.dp,
                            color = dividerColor.copy(alpha = 0.3f),
                            shape = RoundedCornerShape(24.dp)
                        ),
                    colors = CardDefaults.cardColors(
                        containerColor = cardBg
                    ),
                    shape = RoundedCornerShape(24.dp)
                ) {
                    Column(
                        modifier = Modifier.padding(16.dp),
                        verticalArrangement = Arrangement.spacedBy(14.dp)
                    ) {
                        Text(
                            text = "MONITORING PREFERENCES",
                            fontSize = 11.sp,
                            fontWeight = FontWeight.Bold,
                            color = brandPrimary,
                            letterSpacing = 1.sp
                        )

                        // 1. Active Sensor Source Selector
                        Column(verticalArrangement = Arrangement.spacedBy(6.dp)) {
                            Text(
                                text = "Active Temperature Sensor",
                                fontSize = 13.sp,
                                fontWeight = FontWeight.Bold,
                                color = textColor
                            )
                            Row(
                                modifier = Modifier.fillMaxWidth(),
                                horizontalArrangement = Arrangement.spacedBy(6.dp)
                            ) {
                                val sensors = listOf(
                                    "cpu" to "CPU",
                                    "battery" to "Battery"
                                )
                                sensors.forEach { (type, label) ->
                                    val isSelected = sensorType == type
                                    Box(
                                        modifier = Modifier
                                            .weight(1f)
                                            .clip(RoundedCornerShape(8.dp))
                                            .background(
                                                if (isSelected) brandPrimary else {
                                                    if (isDark) Color.White.copy(alpha = 0.06f) else Color.Black.copy(alpha = 0.04f)
                                                }
                                            )
                                            .clickable { viewModel.setSensorType(type) }
                                            .padding(vertical = 8.dp),
                                        contentAlignment = Alignment.Center
                                    ) {
                                        Text(
                                            text = label,
                                            fontSize = 12.sp,
                                            fontWeight = if (isSelected) FontWeight.Bold else FontWeight.Medium,
                                            color = if (isSelected) textOnBrand else textColor
                                        )
                                    }
                                }
                            }
                        }

                        // 2. Polling frequency choose
                        Column(verticalArrangement = Arrangement.spacedBy(6.dp)) {
                            Text(
                                text = "Polling Update Frequency",
                                fontSize = 13.sp,
                                fontWeight = FontWeight.Bold,
                                color = textColor
                            )
                            Row(
                                modifier = Modifier.fillMaxWidth(),
                                horizontalArrangement = Arrangement.spacedBy(6.dp)
                            ) {
                                val frequencies = listOf(
                                    2 to "2s",
                                    5 to "5s",
                                    15 to "15s",
                                    30 to "30s",
                                    60 to "1m"
                                )
                                frequencies.forEach { (sec, label) ->
                                    val isSelected = updateFrequency == sec
                                    Box(
                                        modifier = Modifier
                                            .weight(1f)
                                            .clip(RoundedCornerShape(8.dp))
                                            .background(
                                                if (isSelected) brandPrimary else {
                                                    if (isDark) Color.White.copy(alpha = 0.06f) else Color.Black.copy(alpha = 0.04f)
                                                }
                                            )
                                            .clickable { viewModel.setUpdateFrequency(sec) }
                                            .padding(vertical = 8.dp),
                                        contentAlignment = Alignment.Center
                                    ) {
                                        Text(
                                            text = label,
                                            fontSize = 12.sp,
                                            fontWeight = if (isSelected) FontWeight.Bold else FontWeight.Medium,
                                            color = if (isSelected) textOnBrand else textColor
                                        )
                                    }
                                }
                            }
                        }

                        // 2. Unit system, visual interface choice
                        Row(
                            modifier = Modifier.fillMaxWidth(),
                            horizontalArrangement = Arrangement.spacedBy(12.dp)
                        ) {
                            Column(
                                modifier = Modifier.weight(1f),
                                verticalArrangement = Arrangement.spacedBy(6.dp)
                            ) {
                                Text(
                                    text = "Metric System",
                                    fontSize = 13.sp,
                                    fontWeight = FontWeight.Bold,
                                    color = textColor
                                )
                                Row(
                                    modifier = Modifier
                                        .fillMaxWidth()
                                        .background(
                                            color = if (isDark) Color.White.copy(alpha = 0.04f) else Color.Black.copy(alpha = 0.03f),
                                            shape = RoundedCornerShape(10.dp)
                                        )
                                        .padding(3.dp)
                                ) {
                                    val units = listOf("celsius" to "°C", "fahrenheit" to "°F")
                                    units.forEach { (unit, label) ->
                                        val isSelected = tempUnit == unit
                                        Box(
                                            modifier = Modifier
                                                .weight(1f)
                                                .clip(RoundedCornerShape(8.dp))
                                                .background(
                                                    if (isSelected) brandPrimary.copy(alpha = 0.2f) else Color.Transparent
                                                )
                                                .clickable { viewModel.setTempUnit(unit) }
                                                .padding(vertical = 8.dp),
                                            contentAlignment = Alignment.Center
                                        ) {
                                            Text(
                                                text = label,
                                                fontSize = 12.sp,
                                                fontWeight = if (isSelected) FontWeight.Bold else FontWeight.Medium,
                                                color = if (isSelected) brandPrimary else secondaryText
                                            )
                                        }
                                    }
                                }
                            }

                            Column(
                                modifier = Modifier.weight(1.2f),
                                verticalArrangement = Arrangement.spacedBy(6.dp)
                            ) {
                                Text(
                                    text = "Theme Interface",
                                    fontSize = 13.sp,
                                    fontWeight = FontWeight.Bold,
                                    color = textColor
                                )
                                Row(
                                    modifier = Modifier
                                        .fillMaxWidth()
                                        .background(
                                            color = if (isDark) Color.White.copy(alpha = 0.04f) else Color.Black.copy(alpha = 0.03f),
                                            shape = RoundedCornerShape(10.dp)
                                        )
                                        .padding(3.dp)
                                ) {
                                    val themes = listOf(
                                        "system" to "System",
                                        "dark" to "Dark",
                                        "light" to "Light"
                                    )
                                    themes.forEach { (themeKey, label) ->
                                        val isSelected = themeSetting == themeKey
                                        Box(
                                            modifier = Modifier
                                                .weight(1f)
                                                .clip(RoundedCornerShape(8.dp))
                                                .background(
                                                    if (isSelected) brandPrimary.copy(alpha = 0.2f) else Color.Transparent
                                                )
                                                .clickable { viewModel.setThemeSetting(themeKey) }
                                                .padding(vertical = 8.dp),
                                            contentAlignment = Alignment.Center
                                        ) {
                                            Text(
                                                text = label,
                                                fontSize = 11.sp,
                                                fontWeight = if (isSelected) FontWeight.Bold else FontWeight.Medium,
                                                color = if (isSelected) brandPrimary else secondaryText
                                            )
                                        }
                                    }
                                }
                            }
                        }

                        // 3. Slider alert warning trigger
                        Column(verticalArrangement = Arrangement.spacedBy(4.dp)) {
                            Row(
                                modifier = Modifier.fillMaxWidth(),
                                horizontalArrangement = Arrangement.SpaceBetween,
                                verticalAlignment = Alignment.CenterVertically
                            ) {
                                Text(
                                    text = "Thermal Limit Alarm Trigger",
                                    fontSize = 13.sp,
                                    fontWeight = FontWeight.Bold,
                                    color = textColor
                                )
                                val displayValue = if (tempUnit == "fahrenheit") {
                                    "${(warningThreshold * 9f / 5f + 32f).toInt()}°F"
                                } else {
                                    "${warningThreshold.toInt()}°C"
                                }
                                Text(
                                    text = displayValue,
                                    fontSize = 13.sp,
                                    fontWeight = FontWeight.Black,
                                    color = if (isDark) Color(0xFFFFB4AB) else Color(0xFFBA1A1A)
                                )
                            }
                            Slider(
                                value = warningThreshold,
                                onValueChange = { viewModel.setWarningThreshold(it) },
                                valueRange = 32.0f..65.0f,
                                colors = SliderDefaults.colors(
                                    thumbColor = brandPrimary,
                                    activeTrackColor = brandPrimary,
                                    inactiveTrackColor = if (isDark) Color.White.copy(alpha = 0.15f) else Color.Black.copy(alpha = 0.1f)
                                )
                            )
                        }

                        // 4. Sandbox Demo Mode Switch
                        Row(
                            modifier = Modifier
                                .fillMaxWidth()
                                .background(
                                    color = if (isDark) Color.White.copy(alpha = 0.04f) else Color.Black.copy(alpha = 0.03f),
                                    shape = RoundedCornerShape(12.dp)
                                )
                                .padding(12.dp),
                            verticalAlignment = Alignment.CenterVertically,
                            horizontalArrangement = Arrangement.SpaceBetween
                        ) {
                            Column(modifier = Modifier.weight(0.85f)) {
                                Text(
                                    text = "Enable Sensor Simulation",
                                    fontSize = 13.sp,
                                    fontWeight = FontWeight.Bold,
                                    color = textColor
                                )
                                Text(
                                    text = "Simulates natural temperature variances for emulators where thermal hardware registers are sandboxed.",
                                    fontSize = 11.sp,
                                    color = secondaryText
                                )
                            }
                            Switch(
                                checked = demoMode,
                                onCheckedChange = { viewModel.setDemoModeEnabled(it) },
                                colors = SwitchDefaults.colors(
                                    checkedThumbColor = brandPrimary,
                                    checkedTrackColor = brandPrimary.copy(alpha = 0.4f)
                                )
                            )
                        }
                    }
                }
            }

            // ======== 2. CENTRAL WORKING VIEWPORT CONTAINER ========
            Box(
                modifier = Modifier
                    .weight(1f)
                    .fillMaxWidth()
                    .padding(horizontal = 16.dp, vertical = 8.dp)
            ) {
                when (selectedTab) {
                    0 -> {
                        // --- TAB 0: HOVER DIGITAL THERMAL WIDGET ---
                        val scrollState = rememberScrollState()
                        Column(
                            modifier = Modifier
                                .fillMaxSize()
                                .verticalScroll(scrollState),
                            verticalArrangement = Arrangement.spacedBy(16.dp),
                            horizontalAlignment = Alignment.CenterHorizontally
                        ) {
                            // Professional Polish styling gradient temperature card
                            Card(
                                modifier = Modifier
                                    .fillMaxWidth()
                                    .border(
                                        width = 1.dp,
                                        color = dividerColor.copy(alpha = 0.2f),
                                        shape = RoundedCornerShape(28.dp)
                                    ),
                                colors = CardDefaults.cardColors(
                                    containerColor = cardBg
                                ),
                                shape = RoundedCornerShape(28.dp),
                                elevation = CardDefaults.cardElevation(defaultElevation = 2.dp)
                            ) {
                                Box(
                                    modifier = Modifier
                                        .fillMaxWidth()
                                        .padding(24.dp)
                                ) {
                                    // Soft light glow in background
                                    Box(
                                        modifier = Modifier
                                            .size(240.dp)
                                            .align(Alignment.Center)
                                            .background(
                                                Brush.radialGradient(
                                                    colors = listOf(
                                                        accentColor.copy(alpha = if (isDark) 0.08f else 0.05f),
                                                        Color.Transparent
                                                    )
                                                )
                                            )
                                    )

                                    Column(
                                        horizontalAlignment = Alignment.CenterHorizontally,
                                        verticalArrangement = Arrangement.spacedBy(16.dp),
                                        modifier = Modifier.fillMaxWidth()
                                    ) {
                                        Row(
                                            verticalAlignment = Alignment.CenterVertically,
                                            horizontalArrangement = Arrangement.spacedBy(6.dp)
                                        ) {
                                            DeviceSensorIcon(sensorType = sensorType, tint = brandPrimary, modifier = Modifier.size(16.dp))
                                            Text(
                                                text = if (sensorType == "battery") "CURRENT BATTERY TEMPERATURE" else "CURRENT CPU TEMPERATURE",
                                                fontSize = 11.sp,
                                                fontWeight = FontWeight.Bold,
                                                color = brandPrimary,
                                                letterSpacing = 1.4.sp,
                                                textAlign = TextAlign.Center
                                            )
                                        }

                                        // RADIAL ARC TRACK GAUGE
                                        Box(
                                            modifier = Modifier.size(210.dp),
                                            contentAlignment = Alignment.Center
                                        ) {
                                            val scaleDegree by animateFloatAsState(
                                                targetValue = activeTemp,
                                                animationSpec = spring(
                                                    dampingRatio = Spring.DampingRatioMediumBouncy,
                                                    stiffness = Spring.StiffnessLow
                                                ),
                                                label = "temp_gauge_animation"
                                            )

                                            Canvas(modifier = Modifier.fillMaxSize()) {
                                                val strokeSize = 10.dp.toPx()
                                                val diameter = size.minDimension - strokeSize
                                                val sizeOutline = Size(diameter, diameter)
                                                val offsetOutline = Offset(strokeSize / 2f, strokeSize / 2f)

                                                // Clean background tracking arc
                                                drawArc(
                                                    color = if (isDark) Color.White.copy(alpha = 0.06f) else Color.Black.copy(alpha = 0.06f),
                                                    startAngle = 140f,
                                                    sweepAngle = 260f,
                                                    useCenter = false,
                                                    style = Stroke(width = strokeSize, cap = StrokeCap.Round),
                                                    size = sizeOutline,
                                                    topLeft = offsetOutline
                                                )

                                                // Dynamic Temperature Indicator progress
                                                val fraction = ((scaleDegree - 20f) / (70f - 20f)).coerceIn(0f, 1f)
                                                val sweepAngle = 260f * fraction

                                                drawArc(
                                                    color = accentColor,
                                                    startAngle = 140f,
                                                    sweepAngle = sweepAngle,
                                                    useCenter = false,
                                                    style = Stroke(width = strokeSize, cap = StrokeCap.Round),
                                                    size = sizeOutline,
                                                    topLeft = offsetOutline
                                                )
                                            }

                                            // Gauge center contents
                                            Column(
                                                horizontalAlignment = Alignment.CenterHorizontally,
                                                verticalArrangement = Arrangement.Center
                                            ) {
                                                val displayTempString = if (tempUnit == "fahrenheit") {
                                                    val fah = activeTemp * 9f / 5f + 32f
                                                    String.format(Locale.US, "%.1f", fah)
                                                } else {
                                                    String.format(Locale.US, "%.1f", activeTemp)
                                                }

                                                Row(
                                                    verticalAlignment = Alignment.Bottom
                                                ) {
                                                    Text(
                                                        text = displayTempString,
                                                        fontSize = 58.sp,
                                                        fontWeight = FontWeight.Bold,
                                                        color = primaryText,
                                                        letterSpacing = (-1.5).sp,
                                                        fontFamily = FontFamily.SansSerif
                                                    )
                                                    Text(
                                                        text = if (tempUnit == "fahrenheit") "°F" else "°C",
                                                        fontSize = 22.sp,
                                                        fontWeight = FontWeight.Light,
                                                        color = brandPrimary,
                                                        modifier = Modifier.padding(bottom = 10.dp, start = 2.dp)
                                                    )
                                                }

                                                // State performance badge
                                                val alarmText = when {
                                                    activeTemp >= warningThreshold -> "LIMIT EXCEEDED"
                                                    activeTemp >= warningThreshold - 4.0f -> "ELEVATED ALERT"
                                                    else -> "OPTIMAL COOL"
                                                }
                                                Box(
                                                    modifier = Modifier
                                                        .clip(RoundedCornerShape(100.dp))
                                                        .background(accentColor.copy(alpha = 0.12f))
                                                        .padding(horizontal = 12.dp, vertical = 4.dp)
                                                ) {
                                                    Text(
                                                        text = alarmText,
                                                        fontSize = 10.sp,
                                                        fontWeight = FontWeight.Bold,
                                                        color = accentColor,
                                                        letterSpacing = 0.5.sp
                                                    )
                                                }
                                            }
                                        }

                                        // Sensor source metadata
                                        Text(
                                            text = "Source: ${currentReading?.source ?: "Scanning Subsystem..."}",
                                            fontSize = 11.sp,
                                            fontWeight = FontWeight.Medium,
                                            color = secondaryText,
                                            fontFamily = FontFamily.Monospace,
                                            textAlign = TextAlign.Center
                                        )
                                    }
                                }
                            }

                            // Dynamic Update frequency indicator info bar
                            Box(
                                modifier = Modifier
                                    .fillMaxWidth()
                                    .clip(RoundedCornerShape(16.dp))
                                    .background(cardBg.copy(alpha = 0.6f))
                                    .padding(14.dp)
                            ) {
                                Row(
                                    verticalAlignment = Alignment.CenterVertically,
                                    horizontalArrangement = Arrangement.spacedBy(8.dp)
                                ) {
                                    Icon(
                                        imageVector = Icons.Default.Refresh,
                                        contentDescription = "Refresh frequency indicator",
                                        tint = brandPrimary,
                                        modifier = Modifier.size(16.dp)
                                    )
                                    Text(
                                        text = "Actively monitoring at systematic $updateFrequency seconds interval frequency ticks.",
                                        fontSize = 12.sp,
                                        color = textColor
                                    )
                                }
                            }
                        }
                    }

                    1 -> {
                        // --- TAB 1: SCROLLABLE CHRONOLOGICAL LOG ---
                        Card(
                            modifier = Modifier
                                .fillMaxSize()
                                .border(
                                    width = 1.dp,
                                    color = dividerColor.copy(alpha = 0.2f),
                                    shape = RoundedCornerShape(24.dp)
                                ),
                            colors = CardDefaults.cardColors(
                                containerColor = cardBg
                            ),
                            shape = RoundedCornerShape(24.dp)
                        ) {
                            Column(
                                modifier = Modifier.fillMaxSize()
                            ) {
                                // List Top Header bar
                                Row(
                                    modifier = Modifier
                                        .fillMaxWidth()
                                        .padding(16.dp),
                                    verticalAlignment = Alignment.CenterVertically,
                                    horizontalArrangement = Arrangement.SpaceBetween
                                ) {
                                    Column {
                                        Text(
                                            text = if (sensorType == "battery") "Battery Thermals History" else "CPU Thermals History",
                                            fontSize = 15.sp,
                                            fontWeight = FontWeight.Bold,
                                            color = primaryText
                                        )
                                        Text(
                                            text = if (sensorType == "battery") "Battery logs recorded over the past 24H" else "Processor core logs recorded over the past 24H",
                                            fontSize = 11.sp,
                                            color = secondaryText
                                        )
                                    }

                                    Row(
                                        verticalAlignment = Alignment.CenterVertically,
                                        horizontalArrangement = Arrangement.spacedBy(8.dp)
                                    ) {
                                        // Length tag
                                        Box(
                                            modifier = Modifier
                                                .clip(RoundedCornerShape(6.dp))
                                                .background(brandPrimary.copy(alpha = 0.12f))
                                                .padding(horizontal = 7.dp, vertical = 3.dp)
                                        ) {
                                            Text(
                                                text = "${readings.size} pts",
                                                fontSize = 10.sp,
                                                fontWeight = FontWeight.Bold,
                                                color = brandPrimary
                                            )
                                        }

                                        if (readings.isNotEmpty()) {
                                            IconButton(
                                                onClick = { viewModel.clearHistoryLogs() },
                                                modifier = Modifier
                                                    .size(36.dp)
                                                    .background(
                                                        color = Color(0xFFFFB4AB).copy(alpha = 0.15f),
                                                        shape = CircleShape
                                                    )
                                            ) {
                                                Icon(
                                                    imageVector = Icons.Default.Delete,
                                                    contentDescription = "Factory Reset/Clear Readings",
                                                    tint = if (isDark) Color(0xFFFFB4AB) else Color(0xFFBA1A1A),
                                                    modifier = Modifier.size(16.dp)
                                                )
                                            }
                                        }
                                    }
                                }

                                Divider(color = dividerColor.copy(alpha = 0.3f))

                                if (readings.isEmpty()) {
                                    Column(
                                        modifier = Modifier
                                            .weight(1f)
                                            .fillMaxWidth()
                                            .padding(24.dp),
                                        horizontalAlignment = Alignment.CenterHorizontally,
                                        verticalArrangement = Arrangement.Center
                                    ) {
                                        Icon(
                                            imageVector = Icons.Default.Refresh,
                                            contentDescription = "Gathering sensor nodes",
                                            tint = secondaryText.copy(alpha = 0.4f),
                                            modifier = Modifier.size(48.dp)
                                        )
                                        Spacer(modifier = Modifier.height(12.dp))
                                        Text(
                                            text = "No recorded sensor log entries yet",
                                            fontSize = 14.sp,
                                            fontWeight = FontWeight.Bold,
                                            color = primaryText
                                        )
                                        Text(
                                            text = "Awaiting the next hardware scanner polling sweep. Ensure dynamic simulation is turned on if you are debugging in dry emulators.",
                                            fontSize = 12.sp,
                                            color = secondaryText,
                                            textAlign = TextAlign.Center,
                                            modifier = Modifier.padding(horizontal = 16.dp, vertical = 4.dp)
                                        )
                                    }
                                } else {
                                    LazyColumn(
                                        modifier = Modifier
                                            .weight(1f)
                                            .fillMaxWidth(),
                                        contentPadding = PaddingValues(bottom = 16.dp)
                                    ) {
                                        items(readings, key = { it.id }) { record ->
                                            HistoryLogItem(
                                                record = record,
                                                tempUnit = tempUnit,
                                                warningThreshold = warningThreshold,
                                                isDark = isDark
                                            )
                                            Divider(color = dividerColor.copy(alpha = 0.15f))
                                        }
                                    }
                                }
                            }
                        }
                    }

                    2 -> {
                        // --- TAB 2: DETAILED ANALYTIC STATS & GRAPH ---
                        val scrollState = rememberScrollState()
                        Column(
                            modifier = Modifier
                                .fillMaxSize()
                                .verticalScroll(scrollState),
                            verticalArrangement = Arrangement.spacedBy(16.dp)
                        ) {
                            // High potential summary statement cards (side-by-side)
                            Row(
                                modifier = Modifier.fillMaxWidth(),
                                horizontalArrangement = Arrangement.spacedBy(10.dp)
                            ) {
                                val minVal = minTemp ?: 36.5f
                                val maxVal = maxTemp ?: 36.5f

                                // Min temperature
                                val minTimeString = remember(readings) {
                                    val r = readings.minByOrNull { it.temperature }
                                    if (r != null) {
                                        SimpleDateFormat("HH:mm", Locale.getDefault()).format(Date(r.timestamp))
                                    } else "00:00"
                                }

                                Card(
                                    modifier = Modifier
                                        .weight(1f)
                                        .border(
                                            width = 1.dp,
                                            color = dividerColor.copy(alpha = 0.15f),
                                            shape = RoundedCornerShape(20.dp)
                                        ),
                                    colors = CardDefaults.cardColors(
                                        containerColor = cardBg
                                    ),
                                    shape = RoundedCornerShape(20.dp)
                                ) {
                                    Column(
                                        modifier = Modifier.padding(14.dp),
                                        verticalArrangement = Arrangement.spacedBy(4.dp)
                                    ) {
                                        Text(
                                            text = "24H MINIMUM",
                                            fontSize = 9.sp,
                                            fontWeight = FontWeight.Bold,
                                            color = brandPrimary,
                                            letterSpacing = 1.sp
                                        )
                                        val displayMin = if (tempUnit == "fahrenheit") {
                                            "${String.format(Locale.US, "%.1f", minVal * 9f / 5f + 32f)}°F"
                                        } else {
                                            "${String.format(Locale.US, "%.1f", minVal)}°C"
                                        }
                                        Text(
                                            text = displayMin,
                                            fontSize = 24.sp,
                                            fontWeight = FontWeight.Bold,
                                            color = primaryText
                                        )
                                        Text(
                                            text = "Recorded at $minTimeString",
                                            fontSize = 10.sp,
                                            color = secondaryText
                                        )
                                    }
                                }

                                // Max temperature
                                val maxTimeString = remember(readings) {
                                    val r = readings.maxByOrNull { it.temperature }
                                    if (r != null) {
                                        SimpleDateFormat("HH:mm", Locale.getDefault()).format(Date(r.timestamp))
                                    } else "00:00"
                                }

                                Card(
                                    modifier = Modifier
                                        .weight(1f)
                                        .border(
                                            width = 1.dp,
                                            color = dividerColor.copy(alpha = 0.15f),
                                            shape = RoundedCornerShape(20.dp)
                                        ),
                                    colors = CardDefaults.cardColors(
                                        containerColor = cardBg
                                    ),
                                    shape = RoundedCornerShape(20.dp)
                                ) {
                                    Column(
                                        modifier = Modifier.padding(14.dp),
                                        verticalArrangement = Arrangement.spacedBy(4.dp)
                                    ) {
                                        Text(
                                            text = "24H MAXIMUM",
                                            fontSize = 9.sp,
                                            fontWeight = FontWeight.Bold,
                                            color = if (isDark) Color(0xFFFFB4AB) else Color(0xFFBA1A1A),
                                            letterSpacing = 1.sp
                                        )
                                        val displayMax = if (tempUnit == "fahrenheit") {
                                            "${String.format(Locale.US, "%.1f", maxVal * 9f / 5f + 32f)}°F"
                                        } else {
                                            "${String.format(Locale.US, "%.1f", maxVal)}°C"
                                        }
                                        Text(
                                            text = displayMax,
                                            fontSize = 24.sp,
                                            fontWeight = FontWeight.Bold,
                                            color = if (maxVal >= warningThreshold) accentColor else primaryText
                                        )
                                        Text(
                                            text = "Recorded at $maxTimeString",
                                            fontSize = 10.sp,
                                            color = secondaryText
                                        )
                                    }
                                }
                            }

                            // Interactive Trend Chart Representation
                            Card(
                                modifier = Modifier
                                    .fillMaxWidth()
                                    .border(
                                        width = 1.dp,
                                        color = dividerColor.copy(alpha = 0.2f),
                                        shape = RoundedCornerShape(24.dp)
                                    ),
                                colors = CardDefaults.cardColors(
                                    containerColor = cardBg
                                ),
                                shape = RoundedCornerShape(24.dp)
                            ) {
                                Column(modifier = Modifier.padding(16.dp), verticalArrangement = Arrangement.spacedBy(12.dp)) {
                                    Text(
                                        text = "ANALYTIC THERMAL TRENDS",
                                        fontSize = 11.sp,
                                        fontWeight = FontWeight.Bold,
                                        color = brandPrimary,
                                        letterSpacing = 1.sp
                                    )

                                    if (readings.isEmpty()) {
                                        Box(
                                            modifier = Modifier
                                                .fillMaxWidth()
                                                .height(160.dp),
                                            contentAlignment = Alignment.Center
                                        ) {
                                            Text(
                                                text = "Awaiting statistics data points...",
                                                fontSize = 12.sp,
                                                color = secondaryText
                                            )
                                        }
                                    } else {
                                        TemperatureTrendChart(
                                            readings = readings.take(20),
                                            warningThreshold = warningThreshold,
                                            isDark = isDark
                                        )
                                    }
                                }
                            }

                            // Dynamic hardware protection advice details
                            Card(
                                modifier = Modifier.fillMaxWidth(),
                                colors = CardDefaults.cardColors(
                                    containerColor = brandPrimary.copy(alpha = 0.08f)
                                ),
                                shape = RoundedCornerShape(16.dp)
                            ) {
                                Row(
                                    modifier = Modifier.padding(16.dp),
                                    horizontalArrangement = Arrangement.spacedBy(10.dp),
                                    verticalAlignment = Alignment.CenterVertically
                                ) {
                                    Icon(
                                        imageVector = Icons.Default.Info,
                                        contentDescription = "Tips icon info",
                                        tint = brandPrimary,
                                        modifier = Modifier.size(24.dp)
                                    )
                                    Text(
                                        text = if (sensorType == "battery") {
                                            "Pro Tip: Keeping Battery temperatures beneath ${warningThreshold.toInt()}°C avoids excessive heat stress, slows long-term battery degradation, and ensures high safety margins."
                                        } else {
                                            "Pro Tip: Keeping CPU temperatures beneath ${warningThreshold.toInt()}°C prevents chip throttling and prolongs battery lifespan efficiency."
                                        },
                                        fontSize = 12.sp,
                                        color = brandPrimary,
                                        fontWeight = FontWeight.Medium
                                    )
                                }
                            }
                        }
                    }
                }
            }

            // ======== 3. STICKY PROFESSIONAL BOTTOM NAVIGATION BAR ========
            Divider(color = dividerColor.copy(alpha = 0.3f))
            Surface(
                modifier = Modifier
                    .fillMaxWidth()
                    .height(80.dp),
                color = navBg
            ) {
                Row(
                    modifier = Modifier.fillMaxSize(),
                    verticalAlignment = Alignment.CenterVertically,
                    horizontalArrangement = Arrangement.SpaceAround
                ) {
                    // Item 0: Widget
                    val isWidget = selectedTab == 0
                    Column(
                        modifier = Modifier
                            .weight(1f)
                            .clickable { selectedTab = 0 }
                            .padding(vertical = 4.dp),
                        horizontalAlignment = Alignment.CenterHorizontally,
                        verticalArrangement = Arrangement.spacedBy(4.dp)
                    ) {
                        Box(
                            modifier = Modifier
                                .clip(RoundedCornerShape(100.dp))
                                .background(if (isWidget) brandPrimary else Color.Transparent)
                                .padding(horizontal = 20.dp, vertical = 6.dp)
                        ) {
                            Icon(
                                imageVector = Icons.Default.Home,
                                contentDescription = "Widget view option",
                                tint = if (isWidget) textOnBrand else secondaryText,
                                modifier = Modifier.size(22.dp)
                            )
                        }
                        Text(
                            text = "Widget",
                            fontSize = 11.sp,
                            fontWeight = if (isWidget) FontWeight.Bold else FontWeight.Medium,
                            color = if (isWidget) primaryText else secondaryText
                        )
                    }

                    // Item 1: Log
                    val isLog = selectedTab == 1
                    Column(
                        modifier = Modifier
                            .weight(1f)
                            .clickable { selectedTab = 1 }
                            .padding(vertical = 4.dp),
                        horizontalAlignment = Alignment.CenterHorizontally,
                        verticalArrangement = Arrangement.spacedBy(4.dp)
                    ) {
                        Box(
                            modifier = Modifier
                                .clip(RoundedCornerShape(100.dp))
                                .background(if (isLog) brandPrimary else Color.Transparent)
                                .padding(horizontal = 20.dp, vertical = 6.dp)
                        ) {
                            Icon(
                                imageVector = Icons.Default.List,
                                contentDescription = "History Logs list toggle",
                                tint = if (isLog) textOnBrand else secondaryText,
                                modifier = Modifier.size(22.dp)
                            )
                        }
                        Text(
                            text = "History Log",
                            fontSize = 11.sp,
                            fontWeight = if (isLog) FontWeight.Bold else FontWeight.Medium,
                            color = if (isLog) primaryText else secondaryText
                        )
                    }

                    // Item 2: Stats
                    val isStats = selectedTab == 2
                    Column(
                        modifier = Modifier
                            .weight(1f)
                            .clickable { selectedTab = 2 }
                            .padding(vertical = 4.dp),
                        horizontalAlignment = Alignment.CenterHorizontally,
                        verticalArrangement = Arrangement.spacedBy(4.dp)
                    ) {
                        Box(
                            modifier = Modifier
                                .clip(RoundedCornerShape(100.dp))
                                .background(if (isStats) brandPrimary else Color.Transparent)
                                .padding(horizontal = 20.dp, vertical = 6.dp)
                        ) {
                            Icon(
                                imageVector = Icons.Default.Info,
                                contentDescription = "Performance sensor stats option",
                                tint = if (isStats) textOnBrand else secondaryText,
                                modifier = Modifier.size(22.dp)
                            )
                        }
                        Text(
                            text = "Stats Detail",
                            fontSize = 11.sp,
                            fontWeight = if (isStats) FontWeight.Bold else FontWeight.Medium,
                            color = if (isStats) primaryText else secondaryText
                        )
                    }
                }
            }
        }
    }

    // ======== SENSOR COMPONENT INFO ALERTDIALOG ========
    if (showInfoDialog) {
        AlertDialog(
            onDismissRequest = { showInfoDialog = false },
            confirmButton = {
                TextButton(onClick = { showInfoDialog = false }) {
                    Text(
                        "DISMISS",
                        fontWeight = FontWeight.Bold,
                        color = brandPrimary
                    )
                }
            },
            title = {
                Text(
                    text = "Core Hardware Sensor Monitor",
                    fontWeight = FontWeight.Bold,
                    fontSize = 16.sp
                )
            },
            text = {
                Column(verticalArrangement = Arrangement.spacedBy(10.dp)) {
                    Text(
                        text = "This application connects directly with built-in core kernel hardware registers on the Android root subsystem.",
                        fontSize = 13.sp
                    )
                    Divider(color = dividerColor.copy(alpha = 0.3f))
                    
                    Text(
                        text = "1. Active Sensors: Probes physical file matrices corresponding to on-chip thermal sensors.",
                        fontSize = 12.sp,
                        fontWeight = FontWeight.Bold,
                        color = textColor
                    )
                    Text(
                        text = "2. Active Protection: Tracks values 24/7. Exceeding the Warning trigger sets the alarm to critical warning status.",
                        fontSize = 12.sp,
                        fontWeight = FontWeight.Bold,
                        color = textColor
                    )
                    Text(
                        text = "3. Cache Safety: Designed efficiently. Logs are saved locally in a secure, edge-based Room database and never leave your phone.",
                        fontSize = 12.sp,
                        fontWeight = FontWeight.Bold,
                        color = textColor
                    )
                    
                    Spacer(modifier = Modifier.height(4.dp))
                    Text(
                        text = "Emulator Hint: Virtualized testing environments (like streaming emulators) may block hardware zones. In such cases, use 'Sensor Simulation Mode' in settings to simulate active visual fluctuations.",
                        fontSize = 11.sp,
                        fontStyle = androidx.compose.ui.text.font.FontStyle.Italic,
                        color = brandPrimary
                    )
                }
            },
            shape = RoundedCornerShape(24.dp),
            containerColor = cardBg,
            titleContentColor = primaryText,
            textContentColor = textColor
        )
    }
}

@Composable
fun TemperatureTrendChart(
    readings: List<TemperatureReading>,
    warningThreshold: Float,
    isDark: Boolean
) {
    // Elegant Canvas Drawn Smooth Line Heatmap progression chart
    Canvas(
        modifier = Modifier
            .fillMaxWidth()
            .height(160.dp)
            .padding(top = 10.dp)
    ) {
        val width = size.width
        val height = size.height

        val maxRecorded = readings.maxOfOrNull { it.temperature } ?: 60f
        val minRecorded = readings.minOfOrNull { it.temperature } ?: 25f
        val deltaValue = (maxRecorded - minRecorded).coerceAtLeast(1.0f)

        // Draw guideline intervals
        val gridColor = if (isDark) Color.White.copy(alpha = 0.05f) else Color.Black.copy(alpha = 0.05f)
        val lineCount = 4
        for (i in 0..lineCount) {
            val yOffset = height * (i.toFloat() / lineCount.toFloat())
            drawLine(
                color = gridColor,
                start = Offset(0f, yOffset),
                end = Offset(width, yOffset),
                strokeWidth = 1.dp.toPx()
            )
        }

        if (readings.size > 1) {
            val pointCount = readings.size
            val xStep = width / (pointCount - 1).toFloat()
            val points = readings.asReversed().mapIndexed { index, record ->
                val x = index * xStep
                // Normalize to widget viewport height
                val rawNormalize = (record.temperature - minRecorded) / deltaValue
                val y = height - (rawNormalize * height * 0.75f + height * 0.12f)
                Offset(x, y)
            }

            // Draw connecting gradient graph paths
            val trendPath = androidx.compose.ui.graphics.Path().apply {
                moveTo(points.first().x, points.first().y)
                for (i in 1 until points.size) {
                    lineTo(points[i].x, points[i].y)
                }
            }

            // Flow spline line gradient track
            val lineStrokeColor = if (isDark) Color(0xFFD0BCFF) else Color(0xFF6750A4)
            drawPath(
                path = trendPath,
                color = lineStrokeColor,
                style = Stroke(width = 3.dp.toPx(), cap = StrokeCap.Round)
            )

            // Draw a beautiful fill shape underneath the trend line
            val fillPath = androidx.compose.ui.graphics.Path().apply {
                moveTo(points.first().x, height)
                for (pt in points) {
                    lineTo(pt.x, pt.y)
                }
                lineTo(points.last().x, height)
                close()
            }

            drawPath(
                path = fillPath,
                brush = Brush.verticalGradient(
                    colors = listOf(
                        lineStrokeColor.copy(alpha = 0.25f),
                        lineStrokeColor.copy(alpha = 0.00f)
                    )
                )
            )

            // Render neat active point circles on top
            points.forEachIndexed { idx, pt ->
                val originalTemp = readings.asReversed()[idx].temperature
                val pointColor = if (originalTemp >= warningThreshold) {
                    Color(0xFFFFB4AB)
                } else {
                    lineStrokeColor
                }
                drawCircle(
                    color = pointColor,
                    radius = 4.dp.toPx(),
                    center = pt
                )
            }
        }
    }
}

@Composable
fun CpuChipIcon(
    modifier: Modifier = Modifier,
    tint: Color
) {
    Canvas(modifier = modifier.size(32.dp)) {
        val w = size.width
        val h = size.height
        val margin = w * 0.25f
        
        // Draw the core chip body
        drawRoundRect(
            color = tint,
            topLeft = Offset(margin, margin),
            size = Size(w - 2 * margin, h - 2 * margin),
            cornerRadius = androidx.compose.ui.geometry.CornerRadius(4.dp.toPx(), 4.dp.toPx()),
            style = androidx.compose.ui.graphics.drawscope.Stroke(width = 2.dp.toPx())
        )
        // Fill core slightly
        drawRoundRect(
            color = tint.copy(alpha = 0.3f),
            topLeft = Offset(margin + 3.dp.toPx(), margin + 3.dp.toPx()),
            size = Size(w - 2 * margin - 6.dp.toPx(), h - 2 * margin - 6.dp.toPx()),
            cornerRadius = androidx.compose.ui.geometry.CornerRadius(2.dp.toPx(), 2.dp.toPx())
        )
        
        // Pins on top, bottom, left, right
        val pinCount = 3
        val pinSpacing = (w - 2 * margin) / (pinCount + 1)
        for (i in 1..pinCount) {
            val px = margin + i * pinSpacing
            // Top pins
            drawLine(color = tint, start = Offset(px, margin), end = Offset(px, margin - 4.dp.toPx()), strokeWidth = 2.dp.toPx())
            // Bottom pins
            drawLine(color = tint, start = Offset(px, h - margin), end = Offset(px, h - margin + 4.dp.toPx()), strokeWidth = 2.dp.toPx())
            // Left pins
            drawLine(color = tint, start = Offset(margin, px), end = Offset(margin - 4.dp.toPx(), px), strokeWidth = 2.dp.toPx())
            // Right pins
            drawLine(color = tint, start = Offset(w - margin, px), end = Offset(w - margin + 4.dp.toPx(), px), strokeWidth = 2.dp.toPx())
        }
    }
}

@Composable
fun BatteryIcon(
    modifier: Modifier = Modifier,
    tint: Color
) {
    Canvas(modifier = modifier.size(32.dp)) {
        val w = size.width
        val h = size.height
        
        val bodyWidth = w * 0.45f
        val bodyHeight = h * 0.65f
        val bodyLeft = (w - bodyWidth) / 2f
        val bodyTop = (h - bodyHeight) / 2f + 2.dp.toPx()
        
        // Draw battery main outer body
        drawRoundRect(
            color = tint,
            topLeft = Offset(bodyLeft, bodyTop),
            size = Size(bodyWidth, bodyHeight),
            cornerRadius = androidx.compose.ui.geometry.CornerRadius(3.dp.toPx(), 3.dp.toPx()),
            style = androidx.compose.ui.graphics.drawscope.Stroke(width = 2.dp.toPx())
        )
        
        // Draw anode bump on top
        val bumpWidth = bodyWidth * 0.45f
        val bumpHeight = 3.dp.toPx()
        val bumpLeft = bodyLeft + (bodyWidth - bumpWidth) / 2f
        val bumpTop = bodyTop - bumpHeight
        
        drawRoundRect(
            color = tint,
            topLeft = Offset(bumpLeft, bumpTop),
            size = Size(bumpWidth, bumpHeight),
            cornerRadius = androidx.compose.ui.geometry.CornerRadius(1.dp.toPx(), 1.dp.toPx())
        )
        
        // Fill battery with some charge level inside
        val fillPadding = 3.dp.toPx()
        val fillWidth = bodyWidth - 2 * fillPadding
        val fillHeight = bodyHeight - 2 * fillPadding
        
        drawRoundRect(
            color = tint.copy(alpha = 0.6f),
            topLeft = Offset(bodyLeft + fillPadding, bodyTop + fillPadding),
            size = Size(fillWidth, fillHeight),
            cornerRadius = androidx.compose.ui.geometry.CornerRadius(1.dp.toPx(), 1.dp.toPx())
        )
    }
}

@Composable
fun DeviceSensorIcon(
    sensorType: String,
    tint: Color,
    modifier: Modifier = Modifier
) {
    if (sensorType == "battery") {
        BatteryIcon(modifier = modifier, tint = tint)
    } else {
        CpuChipIcon(modifier = modifier, tint = tint)
    }
}

@Composable
fun ThermostatIcon(
    modifier: Modifier = Modifier,
    tint: Color
) {
    Canvas(modifier = modifier.size(32.dp)) {
        val width = size.width
        val height = size.height

        val bulbRadius = width * 0.22f
        val bulbX = width * 0.5f
        val bulbY = height * 0.72f

        val tubeWidth = width * 0.16f
        val tubeLeft = width * 0.5f - tubeWidth / 2f
        val tubeTop = height * 0.15f

        // Draw outline tube
        val nativePath = android.graphics.Path().apply {
            moveTo(tubeLeft, bulbY)
            lineTo(tubeLeft, tubeTop + tubeWidth / 2f)
            arcTo(
                tubeLeft, tubeTop, tubeLeft + tubeWidth, tubeTop + tubeWidth,
                180f, 180f, false
            )
            lineTo(tubeLeft + tubeWidth, bulbY)
        }

        drawContext.canvas.nativeCanvas.drawPath(nativePath, android.graphics.Paint().apply {
            color = tint.toArgb()
            style = android.graphics.Paint.Style.STROKE
            strokeWidth = 2.dp.toPx()
            isAntiAlias = true
            strokeCap = android.graphics.Paint.Cap.ROUND
        })

        // Draw bulb core circle
        drawCircle(
            color = tint,
            radius = bulbRadius,
            center = Offset(bulbX, bulbY)
        )

        // Light reflection bulb shine
        drawCircle(
            color = Color.White.copy(alpha = 0.5f),
            radius = bulbRadius * 0.35f,
            center = Offset(bulbX - bulbRadius * 0.25f, bulbY - bulbRadius * 0.25f)
        )

        // Active fluid inside tube representing level
        drawRoundRect(
            color = tint,
            topLeft = Offset(tubeLeft + 1.5.dp.toPx(), height * 0.38f),
            size = Size(tubeWidth - 3.dp.toPx(), bulbY - height * 0.38f),
            cornerRadius = androidx.compose.ui.geometry.CornerRadius(tubeWidth / 2f, tubeWidth / 2f)
        )
    }
}

@Composable
fun HistoryLogItem(
    record: TemperatureReading,
    tempUnit: String,
    warningThreshold: Float,
    isDark: Boolean
) {
    val date = Date(record.timestamp)
    val timeFormatter = remember { SimpleDateFormat("HH:mm:ss", Locale.getDefault()) }
    val formattedTime = timeFormatter.format(date)

    // Palette highlight matches
    val brandPrimary = if (isDark) Color(0xFFD0BCFF) else Color(0xFF6750A4)
    val itemColor = when {
        record.temperature >= warningThreshold -> Color(0xFFFFB4AB) // Coral Pink Warning
        record.temperature >= warningThreshold - 4.0f -> Color(0xFFFFCC00) // Amber Warning
        else -> brandPrimary
    }

    Row(
        modifier = Modifier
            .fillMaxWidth()
            .padding(horizontal = 16.dp, vertical = 12.dp),
        horizontalArrangement = Arrangement.SpaceBetween,
        verticalAlignment = Alignment.CenterVertically
    ) {
        Row(
            verticalAlignment = Alignment.CenterVertically,
            horizontalArrangement = Arrangement.spacedBy(12.dp)
        ) {
            Box(
                modifier = Modifier
                    .size(8.dp)
                    .clip(CircleShape)
                    .background(itemColor)
            )

            Column {
                Text(
                    text = formattedTime,
                    fontSize = 13.sp,
                    fontWeight = FontWeight.Bold,
                    fontFamily = FontFamily.Monospace,
                    color = if (isDark) Color.White else Color(0xFF1C1B1F)
                )
                Text(
                     text = "[${record.sensorType.uppercase(Locale.ROOT)}] ${record.source}",
                     fontSize = 11.sp,
                     color = if (isDark) Color(0xFFCAC4D0) else Color(0xFF49454F)
                )
            }
        }

        val displayTemp = if (tempUnit == "fahrenheit") {
            "${String.format(Locale.US, "%.1f", record.temperature * 9f / 5f + 32f)}°F"
        } else {
            "${String.format(Locale.US, "%.1f", record.temperature)}°C"
        }

        Text(
            text = displayTemp,
            fontSize = 15.sp,
            fontWeight = FontWeight.Bold,
            fontFamily = FontFamily.Monospace,
            color = itemColor
        )
    }
}

@Composable
fun Greeting(name: String, modifier: Modifier = Modifier) {
    Text(text = "Hello $name!", modifier = modifier)
}
