# Capital match Hiring Coding Challenge

### Step 1
Allocate `initialFunding` for each bill in each district

1. Map over district, if `billSpecificFunding` exists,
insert the name and the amount; else, get the `categoryDefaultFundingAmount`.

> Now you have each bill's amount in each district

```
[{
    district . name
    bill . name
    bill . category
    bill . amount
} ... ]
```

### Step 2
Increase the fund til the lowest cap while satisfying the principle of proportionality.

1. Find the smallest ratio of category fund cap to category total funding amount.

2. Multiply all bill with the smallest ratio.

> Now you have unchecked funding for each bill in each district

### Step 3
Check whether the total funding of each district exceeds the `availableFunds` or not.

1. Calculate the totalFundingAmount from bills
2. If totalFundingAmount > `availableFunds`, calculate the ratio (avalableFunds / totalFundingAmount) and multiply to all the bill in this district.

> Now you have funding for each bill in each district 

### Step 4
Check if the totalFundingAmount of each bill exceeds the `fundsNeeded` or not.

1. From
```
[{
    district . name
    bill . name
    bill . category
    bill . amount
} ... ]
```
sum the amount and becomes this
```
[{
    bill . name
    bill . amount
} ... ]
```

2. If `bill . amount` > `fundsNeeded` then
find the largest ratio of `fundsNeeded` to `bill . amount`.

3. Multiply all the bill with the ratio.

### Step 5
Print the output. Using the `Data.Tree` `drawTree` function. And the output is shown like this:

![](/resources/output.png)

---

> Zheng Yan PEH / Dec 19 2019
![](/resources/i_did_it.jpg)