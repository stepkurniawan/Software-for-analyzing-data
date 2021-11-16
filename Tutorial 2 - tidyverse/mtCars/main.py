# This is a sample Python script.

# Press Umschalt+F10 to execute it or replace it with your code.
# Press Double Shift to search everywhere for classes, files, tool windows, actions, and settings.


def print_hi(name):
    # Use a breakpoint in the code line below to debug your script.
    print(f'Hi, {name}')  # Press Strg+F8 to toggle the breakpoint.

# Press the green button in the gutter to run the script.
if __name__ == '__main__':
    print_hi('PyCharm')

# See PyCharm help at https://www.jetbrains.com/help/pycharm/

# ------------------------------
# my code
# -----------------------------------

# importing csv
import csv
with open('mtcars.csv', newline='') as csvfile:
    mtcars_reader = csv.reader(csvfile, delimiter= ",")
    for row in mtcars_reader:
        print(row)

print(mtcars_reader)

