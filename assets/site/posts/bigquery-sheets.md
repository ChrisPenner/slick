---
title: "Query a Google Sheets Spreadsheet from BigQuery"
author: Chris Penner
date: Jun 2, 2018
tags: [programming, workflow]
description: A guide to using a google sheets spreadsheet as a BigQuery datasource
image: postman/mail.jpg
---

In line with google's goal to have the whole world running inside a google-doc
by 2050 they've added a new feature to Big Query which allows you query directly
from a Google Spreadsheet! That's right, it reads directly from the sheet so you 
don't need to worry about keeping your bigquery tables up to date.

First I want to stress that we should AVOID USING GOOGLE SHEETS AS A PRIMARY
DATASTORE whenever possible, but sometimes you've just got a bunch of data that
you'd like to run some queries on; this was the case for me earlier; and this is a
great solution for how to do that.

## Prepping The Sheet

BQ has a few quirks, it can (currently) ONLY query the FIRST SHEET of a google sheet, and
doesn't do any special handling of the header row of your spreadsheet. There's
a trick you can do to mitigate this though. Either make a new sheet as the
first sheet and read from the 'real' sheet and drop the headers. If your other
sheet is named MYDATA then you could use something like
`=FILTER(MYDATA!A2:A, NOT(ISBLANK(MYDATA!A2:A)))` which imports every non-blank
row from the MYDATA sheet after dropping the first row.

If you don't want to edit a sheet directly, you can make a new google sheet and
use the [IMPORTRANGE](https://support.google.com/docs/answer/3093340) command
to import the data from a different spreadsheet.

## Creating a Dataset

First step is to create a new bigquery dataset; go to bigquery, select your google cloud project on the left (or create
one if you need to); then create a new 'dataset' in that project we'll set up to sync with our spreadsheet.

![BQ UI](/images/bigquery-sheets/new_table_screen.png)

Now we'll see this screen:

![Create BQ Table](/images/bigquery-sheets/new_bq_table.png)

1. Choose 'Google Drive' as your Location
2. Paste the url of your spreadsheet in the box (just copy it from the url bar when you're at the spreadsheet)
3. Set File Format to Google Sheets
4. Add a table name like you normally would

## Schema

In regards to adding a schema, BigQuery does NOT infer this for you, so you'll
have to add one yourself. Each field of the schema corresponds to a column of
the spreadsheet. For small spreadsheets you can enter it by hand, for bigger
spreadsheets you can just generate a schema definition by copying the headers
row from your spreadsheet and running it through this script:
[HERE](https://gist.github.com/ChrisPenner/2525b29f49cdb6613175cda8c85cb585),
then click 'Edit as Text' by the schema definiton and paste in the result

Lastly, hit 'Create Table'

## Querying

You're good to go now, query away! Note that you won't have the 'Preview' button like other
data tables, this is because no data is actually located in BQ, it streams data
from sheets whenever you make a query. This means the data will always be kept
up to date!

## Breaking Changes

BQ queries the spreadsheet directly so the data will always be up to date, but
this also means that if someone shuffles around columns that the schema will be
out of date with the data. Just note that if the column ordering changes you'll
have to update your schema to match.

Hope that helps, cheers!
