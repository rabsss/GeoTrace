# Contributing to GPS Logger

🎉 Thank you for your interest in contributing to **GPS Logger**!  
Your help makes this project better for everyone — whether you’re fixing a bug, adding a feature, or improving documentation.

---

## 🧩 How to Contribute

There are several ways you can contribute:

- Report bugs or suggest improvements via [GitHub Issues](https://github.com/rabsss/GPS_Logger/issues)
- Submit pull requests for new features or bug fixes
- Improve or clarify documentation
- Share ideas for project enhancements

---

## 🛠️ Development Setup

### 1. Fork and Clone the Repository

```bash
git clone https://github.com/<your-username>/GPS_Logger.git
cd GPS_Logger
```

### 2. Set Up the Environment

Ensure that you have the following installed:

 * Linux (recommended)
 * **[Lambdanative](https://github.com/part-cw/lambdanative/wiki/Getting-Started)**
 * Android **SDK** and **NDK**
 * **Java JDK 8+**
 * **adb** (Android Debug Bridge)

### 3. Build the Project

```bash
./configure GPS_Logger android
make
```

Install on your device:
```bash
adb install path/to/GPS_Logger.apk
```

## 📏 Coding Guidelines
Please follow these general guidelines for code quality and consistency:
| Area        | Guideline                                                     |
|------------|---------------------------------------------------------------|
| Language    | Scheme (LambdaNative syntax)                                   |
| Indentation | 2 spaces — no tabs                                            |
| Comments    | Add clear and concise comments for functions and logic       |
| Commits     | Make small, focused commits that address a single concern    |

## 🧾 Commit Message Format

Format: 
```bash 
<type>: <short summary>
```

**Common Types:**
 * feat: — New feature
 * fix: — Bug fix
 * docs: — Documentation change
 * refactor: — Code restructuring
 * style: — Code style/formatting
 * test: — Adding or modifying tests

## 🚀 Pull Request Process
1. Ensure your branch is up to date with main:
```bash
git pull origin main
```
2. Run a clean build and test your changes.
3. Commit using clear messages.
4. Push your branch:
```bash
git push origin feature/your-feature-name
```
5. Open a Pull Request and provide:
    * A clear title and summary
    * A short description of what you changed

## 💬 Need Help?
If you get stuck or have questions:
 * Open a [Discussion](https://github.com/rabsss/GPS_Logger/discussions)
 * Or Open an issue via [Issues](https://github.com/rabsss/GPS_Logger/issues)
   
Your PR will be reviewed, and feedback may be provided before merging.

## ❤️ Code of Conduct
Please be respectful, collaborative, and constructive in all communications.
All contributors are expected to follow open-source community standards.
