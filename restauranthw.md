# Restaurant Homework Assignment

After analyzing the ordering patterns of your clientelle we have a few recomendations to help improve sales and the customer experience. Below is the chart of the entrees and their optimal wine pairing based off of what your customers ordered most frequently. The confidence you see below is how often an order containing that meat entree also contained that wine (i.e. a value of 0.50 means that 50% of the time that meat is ordered so too is that wine). The lift value is how many times more likely you are to see that wine purchase with that meat than you are to see that wine purchase in general. For instance, the Blackstone Merlot is 6.25 times for likely to be ordered with the Filet Mignon than the Blackstone Merlot is to be ordered in general

| Meat | Wine | Confidence | Lift |
|---- |---- |---- |---- |
| Filet Mignon | Blackstone Merlot | 0.826 | 6.25 |
| Roast Chicken | Duckhorn Chardonnay | 0.802 | 2.75 |
| Duck | Duckhorn Chardonnay | 0.719 | 2.46 |
| Pork Tenderloin | Cantina Pinot Bianco | 0.926 | 1.68 |

We recommend listing these wines as the suggested pairs for the corresponding meat entree. Additionally, the Meiomi Pinot Noir is the least frequently ordered wine by quite a bit, so we recommend that, if possible, this wine be replaced with some sort of rotating, limited time offering, both to boost sales and remind customers that a fourth wine option exists.

We also analyzed your most popular dishes. The most popular entree was the Pork Tenderloin. The most popular complete meal was the Pork Tenderloin with the Roasted Root Vegtables as the side and the Cantina Pinot Bianco as the wine. We recommend that the dish you include in your ad be either this dish or the Filet Mignon, Blackstone Merlot, and Roasted Root Vegetables. Furthermore, as seen in the table above the Filet Mignon and Blackstone Merlot are a very strong pairing.While this is a much less popular dish than the first one mentioned, this is presumably a more expensive dish, and advertising it may induce more customers to try it, boosting your revenue. Either option would be a good choice.

As a final note: not all of the orders were as simple as three items (side, meat, wine). There were order ids with more than three items (388 orders to be exact). Whether this is a data entry error or an accurate representation of your customer behavior is unkown, but it is very unlikely it affected the analysis in any way.
