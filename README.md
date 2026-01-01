# GPS Logger

**GPS Logger** is an open-source Android application that records your device's GPS location in real-time.  
It’s lightweight, accurate, and built using **[LambdaNative](https://github.com/part-cw/lambdanative)** — a cross-platform Scheme-based framework for mobile and embedded applications.

---

## 🚀 Features

- ✅ Logs GPS coordinates (latitude, longitude)
- 🕒 Timestamped entries for each location fix
- 💾 Saves data locally in a structured format
- 🗺️ Easy to export and visualize with external tools (e.g., GIS, Google Earth)
- 📱 Android support via LambdaNative

---

## 🧭 App Overview

The app continuously tracks GPS position once started and logs data to a local file.  

| Component | Description |
|------------|--------------|
| **gps.scm** | Core logic for reading GPS data |
| **main.scm** | Application entry point and UI |
| **gps_log.csv** | Gps logged data storage file |


---

## 🛠️ Building from Source

### Prerequisites

You need:
- **Linux** (recommended)
- **LambdaNative** installed and configured  
  > See: [LambdaNative installation guide](https://github.com/part-cw/lambdanative/wiki/Getting-Started)
- **Android SDK** and **NDK**
- **Java JDK 8+**

### Clone the Repository

```bash
git clone https://github.com/rabsss/GPS_Logger.git
cd GPS_Logger
```

### Build the Application

```bash
./configure GPSLogging android
make
```
The generated APK will be located at:
~/.cache/lambdanative/packages/ on **Linux**
~/Library/Caches/lambdanative/packages/ on **Mac**

### Install the Application

Using **adb**:

```bash
adb install path/to/GPSLogging.apk
```

## Contributing
Contributions are welcome!
If you’d like to improve this project, follow these steps:

1. Fork the repo

2. Create a branch for your feature
   ```bash
      git checkout -b feature/new-feature
   ```
3. Commit your changes
   ```bash
      git commit -m "Add new feature"
   ```

4. Push and open a Pull Request
   Before contributing, please read the [CONTRIBUTING.md](https://github.com/rabsss/GPS_Logger?tab=contributing-ov-file) for guidelines.

## License
This project is licensed under the MIT License - see the [LICENSE](https://github.com/rabsss/GPS_Logger?tab=MIT-1-ov-file) for details.

## Contact 
 * Author - [rabsss](https://github.com/rabsss)
