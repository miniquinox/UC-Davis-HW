(1) Find All Phone Numbers: Write a regex to match all phone numbers in the format "xxx-xxxx-xx" (e.g., "542-6763-58" should match).

Answer:    ([0-9]{3})-([0-9]{4})-([0-9]{2})
Explanation: Numbers 0-9 in sets of 3, 4, and 2 with a - in between.

(2) Extract All Occupations: Use a regex to find and extract all occupation names (e.g., "Interpreter" should be the only thing remaining in the line).

Answer:    (?<=^([^,]+,){6})[^,]*
Explanation: Looks for 8th field in the csv by starting at the beginning of each line and counting 6 commas, then it captures whatever is up to the next comma.

(3) Mask Email Addresses: Replace all email addresses with a generic "[email protected]" while leaving the rest of the data unchanged (e.g., "a.johnston@gmail.com" should become "[email protected]").

Answer:    [-._0-9a-z]*@[-._0-9a-z]+
Replacement:    [email protected]
Explanation: Any number of characters in the set [-._0-9a-z] followed by @ followed by any number of characters in the set [-._0-9a-z].

(4) Transform "Marital Status": Change the "Marital Status" column from 'Single' to 'S' and 'Married' to 'M' while leaving everything else as is.

Answer:    ingle|arried
Explanation: Search for ingle or arried and replace with nothing. This successfully deletes anything after the first character.

(5) SLIGHTLY MODIFIED: Anonymize Last Names: Replace all last names with the first letter followed by 4 asterisks (e.g., "Johnston" -> "J****").

Answer:    (?<=^([^,]+,){1}..)[^,"]*
Replacement:    ****
Explanation: Find text from first comma to following ", and extract only from third character and up.

(6) Extract Ages and Corresponding Emails: Write a regex to extract only the "Age" and "Email" columns, separated by a comma (e.g., "18,a.johnston@gmail.com").

Answer:    ([0-9]*)+","+([._%+-A-Za-z0-9]+@[.-A-Za-z0-9]+\.[A-Za-z]{2,})
Explanation: Make two groups and remove the "," in between.

(7) Reverse the "Experience (Years)": Reverse the digits in the "Experience (Years)" column (e.g., "4" becomes "4", "12" becomes "21") while leaving everything else unchanged.

Answer:   (?<=[A-Z][a-z]*",")([0-9])([0-9]*)(?=","[0-9]*")
Replacement: $2$1
Explanation: Finds number and splits (groups) it into two integers (adds * for 0 or if any), then it swaps groups.

(8) Format "Phone" as International: Transform the phone numbers into an international format by adding "+1-" at the start (e.g., "542-6763-58" becomes "+1-542-6763-58").

Answer:    ([0-9]{3})-([0-9]{4})-([0-9]{2})
Replacement:     +1-$1-$2-$3
Explanation: Look for numbers in format 3 numbers, then -, then 4 numbers, then - and then two numbers. Replace with +1- then first group, then - and second group, then - and third group.

(9) Create "Username" from First Name, Last Name, and Phone: Generate a new column that combines the first letter of the first name, the full last name, and the last two digits of their phone number (e.g., Aston Johnston whose phone number is 542-6763-58 becomes "AJohnston58").

Answer:    "([A-Z])([a-z]*)","([A-Za-z]+)","(.*?)","(.*?)","(.*?)","(\d{3}-\d{4}-)(\d{2})"

Replacement:    "$1$3$8","$1$2","$3","$4","$5","$6","$7$8"
Explanation: Find first letter of the first name, full last name, and two last numbers of phone number by making groups and putting such groups together after.

(10) Reformat "Last Seen" to Day-Month-Year Format: Change the "Last Seen" column to day-month-year format (e.g., "08/23/2012" should be changed to "23-08-2012").

Answer:    ([0-9]{2})/([0-9]{2})/([0-9]{4})
Replacement:    $2-$1-$3
Explanation: Look for pattern NN/NN/NNNN and replace with Second group - first group - third group.
