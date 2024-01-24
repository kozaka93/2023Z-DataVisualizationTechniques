## Project Overview

This [project](https://github.com/fantasy2fry/linux_me_project) was done as part of the [Data Visualization Techniques](https://github.com/kozaka93/2023Z-DataVisualizationTechniques) course. \
To check how our project looks, [click here](https://fantasy2fry.shinyapps.io/linux_me_project/). \
It was created by [Norbert Frydrysiak](https://github.com/fantasy2fry), [Mateusz Karandys](https://github.com/vecel) and [Jakub Kaproń](https://github.com/kuba-kapron).

Kuba (Jakub), Mateusz, and Norbert are avid enthusiasts of Linux operating systems. We thoroughly enjoy working in the terminal and programming in languages such as Java, Python, and R. Hence, we found it intriguing to analyze our data in this realm.

### Bash History

In this section, you can delve into the data related to the commands we input in the terminal.

### System Packages

The System Packages section provides insights into the system packages installed on our computers.

### Git Version Control

Explore our commits and repositories on our computers in the Git Version Control section.

### Python and R Packages

These sections offer an interesting comparison of the packages we use in Python and R.

## How have we gathered the data?

### System installed packages:
For debian based linux distros:
```bash
apt list --installed > installed_packages.txt
```
or
```bash
dpkg -l > installed_packages.txt
```

For arch linux based linux distros:
```bash
pacman -Q > installed_packages.txt
```

For MacOS:
```bash
brew list > file.txt
```

### Commands history:
```bash
history > file.txt
```

### Python packages:
```bash
pip freeze > file.txt
```
For dependencies:
```bash
pip3 install pipdeptree
pipdeptree --json > file.json
```

### Git stats:
```
find ~/ -name .git -execdir sh -c 'echo "Repository $(realpath "{}")" && git log -n 10000 --pretty=format:"%h, %an, %as, %s" --date=short && echo "\n"' \; > file.txt
```

### R packages:
To start a R session type `R` in the terminal and then follow with:
```r
write.table(installed.packages(), "file.txt", sep=",")
q()
```
