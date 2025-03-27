# BlinkR

## Table of Contents

- [Overview](#overview)
- [How to Use BlinkR](#how-to-use-blinkr)
  - [Google Drive Integration](#google-drive-integration)
  - [Menu Bar](#menu-bar)
  - [Introduction & Login](#introduction--login)
- [Background](#background)
- [Hypotheses](#hypotheses)
- [Protocol](#protocol)
  - [Your Protocol](#your-protocol)
  - [Class Protocol](#class-protocol)
  - [Data Hazards](#data-hazards)
- [Measurements](#measurements)
- [Raw Data](#raw-data)
- [Playground](#playground)
- [Analysis Dashboard](#analysis-dashboard)
- [Prepare Data](#prepare-data)
- [Summarise the Data](#summarise-the-data)
- [Create a Figure](#create-a-figure)
- [Statistical Analysis](#statistical-analysis)
- [Writing Up Advice](#writing-up-advice)
- [AI Use Guidance](#ai-use-guidance)
- [Write Up](#write-up)
- [Upload Final Report](#upload-final-report)
- [Simulated Experiment](#simulated-experiment)
- [Feedback](#feedback)
- [Admin Area](#admin-area)
- [Set Up and Deployment](#-set-up-and-deployment)
- [Code Structure](#-code-structure)
- [Final Notes](#-final-notes)

## Overview

BlinkR is a web app written in **Shiny for R**, designed to help students plan, run, and analyse an experiment. It aims to teach the fundamentals of experimental design, good research practices, and basic statistical analysis in R.

The app runs entirely in **R**, and instead of relying on complex databases like SQL, it uses **Google Sheets** and **Google Drive** for persistent storage—making it more accessible and easier to manage.

Originally developed for an experiment comparing blinks per minute under stressed and control conditions (hence the name), BlinkR is now generalizable to any experiment comparing two groups.

**BlinkR can be used for experiments that compare two groups via a T-test or Wilcoxon signed-rank test (depending on normality), with three technical replicates. It supports both paired and unpaired data.**

---

## 👁️ How to Use BlinkR

View the demo app here: [BlinkR Demo](https://ds1405.shinyapps.io/blinkr_app/)

### Google Drive Integration

BlinkR uses **Google Drive** for persistent storage. Upon login, each group is assigned a unique 4-digit user ID, which is used to create a session folder within Google Drive. This folder stores text, code, analysis, and figures, and can be accessed via the **"View Google Drive"** button.

This provides:

1. Backup of all work completed
2. Easy cross-device access (desktop, tablet, or smartphone)

Especially useful in shared environments (e.g., computer labs), this system avoids the need for local downloads.

The modal window that opens from the "View Google Drive" button contains a dropdown selector and download button.

---

### Menu Bar

<img width="339" alt="BlinkR_Sidebar" src="https://github.com/user-attachments/assets/a8e12597-eff5-4830-87f4-95c224876990" />

https://github.com/user-attachments/assets/13a18c21-0278-4f52-93a3-c586577472e2

Once authenticated, users see the full menu bar. At the top, the Group ID is displayed. The sidebar can be opened and closed using the hamburger icon in the top left and includes a **Logout** button and **View Google Drive** button (described above).

---

### Introduction & Login

<img width="1565" alt="BlinkR_Intro_Log_in" src="https://github.com/user-attachments/assets/8c543237-a0a9-4a1e-ad3f-139e74f240c5" />

https://github.com/user-attachments/assets/f417d055-1070-40d7-a967-6505af0f9478

On launching the app, users land on the **Introduction** tab with a login block:

- **Log in**: Select "I have a Group ID" and enter the 4-digit ID.
- **Sign up**: Enter a unique 4-digit group ID and name, or generate one randomly. If the entered ID already exists, an error will be shown.

Once login or sign-up is complete, the login block disappears and the rest of the app becomes accessible.

## https://github.com/user-attachments/assets/2e136476-471f-4321-8ebd-fd86aa053afc

## Background

This section guides students in starting background research, including how to search databases like PubMed.

> _No user input required._

---

## Hypotheses

https://github.com/user-attachments/assets/4f12a2b1-328d-4fed-82a9-9405f29a29a1

Guidance is provided for writing:

- A plain language hypothesis
- A null hypothesis (H0)
- An alternative hypothesis (HA)

Users enter these in three input boxes and click **"Save Notes"**, which stores them in their Google Drive folder.

---

## Protocol

https://github.com/user-attachments/assets/c9f63872-bd88-43c8-9d2a-108fbf1f9538

Split into:

### Your Protocol

Users can brainstorm their experimental design in input fields. This can be shared with an educator via Google Sheets or used independently.

### Class Protocol

Shared selectively by the admin (via user base or admin panel). When active, this appears as a read-only tab for users.

### Data Hazards

Students are prompted to consider potential data hazards ([more info](https://datahazards.com/index.html)). Hazards can be dragged into a "Selected Hazards" box, then submitted as a PDF to Google Drive. This file can be overwritten.

---

## Measurements

https://github.com/user-attachments/assets/a0868a4b-0ed0-4283-b61a-6d0b7260fb63

Users add participants, generating unique IDs. Each participant has a "Measurements" box with three tabs:

1. **Consent Agreement** (must be ticked before submitting data)
2. **Level 1 Measurement** (user-defined variable)
3. **Level 2 Measurement** (user-defined variable)

Each measurement box supports three technical replicates. Data is submitted to a Google Sheet and can be overwritten with a warning.

https://github.com/user-attachments/assets/68f81347-07e1-4b34-8ff2-e058aa6e8857

---

## Raw Data

<img width="1659" alt="BlinkR_Raw_Data" src="https://github.com/user-attachments/assets/05c5f92e-eb3a-4666-af3b-19059464015f" />

Two views:

- **Your Group Data**: Editable, showing only the group’s own entries
- **Class Data**: Aggregated data from all groups, visible only if admin grants access

---

## Playground

https://github.com/user-attachments/assets/47222550-d012-457c-87c5-0d364be0acd7

First exposure to R code, including:

- Virtual code editor
- Predefined code sender
- Run and clear buttons
- Dynamic console output

Encourages users to practice basic R commands.

---

## Analysis Dashboard

<img width="1659" alt="BlinkR_Analysis_Dashboard_empty" src="https://github.com/user-attachments/assets/320ab7e5-0282-4136-935a-e2f6a3a22040" />

Starts with shortcut buttons (e.g., "Prepare Data", "Statistical Analysis"). As results are generated, this dashboard becomes populated.

---

## Prepare Data

https://github.com/user-attachments/assets/87fc2f8e-c873-41c3-acf8-133b32a4c8be

1. **View Data**: Uses `head(data)` with comprehension checks.
2. **Prepare Data**: Averages technical replicates using `dplyr`, based on user-defined variables.

---

## Summarise the Data

https://github.com/user-attachments/assets/98c20120-6558-4f1f-8834-da44c65b914d

Users learn to calculate summary statistics (mean, SD, SEM, etc.) step-by-step using `dplyr`. Includes quizzes and comprehension checks. Final result is saved to the dashboard and Google Drive.

## https://github.com/user-attachments/assets/4c51e0a9-fb91-40e1-bc18-c9dc0752e092

## Create a Figure

https://github.com/user-attachments/assets/d5daa338-c34f-4bdb-988f-f263cba9103d

Users can create a bar chart or box plot (with scatter), customize title/labels/colors, and save to dashboard + Google Drive.

---

## Statistical Analysis

https://github.com/user-attachments/assets/a8d35939-f1e9-4dbe-8364-5db8f9013692

Step-by-step flow:

1. Generate histogram for normality check
2. Select "Normal" or "Not Normal"
3. Select paired/unpaired
4. Perform appropriate test:
   - Normal Unpaired → T-test
   - Normal Paired → Paired T-test
   - Not Normal Unpaired → Wilcoxon rank-sum
   - Not Normal Paired → Wilcoxon signed-rank
5. Effect size (shown only if p ≤ 0.05)
6. Interpretation box (p-value, effect size, one-sentence conclusion)

All results are saved to dashboard + Google Drive.

---

## Writing Up Advice

<img width="1659" alt="BlinkR_Writing_Advice" src="https://github.com/user-attachments/assets/316bcb1e-5490-43ac-bd05-761c17129238" />

Provides advice on writing, collaboration, and general tips.

> _No user input required._

---

## AI Use Guidance

<img width="1659" alt="BlinkR_AI" src="https://github.com/user-attachments/assets/984defd0-7b9f-46a2-bd66-77ca27c700fa" />

Outlines appropriate and inappropriate use of AI for writing the report, including academic integrity warnings.

> _No user input required._

---

## Write Up

https://github.com/user-attachments/assets/0b4562b1-55ec-4af6-9a4c-8e4cea3a95b1

Provides user input boxes for report notes and outlines. All notes are saved to Google Drive.

> _Notes cannot be overwritten._

---

## Upload Final Report

<img width="1659" alt="BlinkR_Upload" src="https://github.com/user-attachments/assets/605190da-4167-4415-bb99-4a54c6b6c441" />

Optional area to upload final reports. Files are saved with group ID and timestamp to Google Drive. Admin can view submissions.

---

## Simulated Experiment

Walkthrough using dummy data, illustrating each step of an experiment. Includes AI-generated report critique.

> _Nothing here is saved to Google Drive._

---

## Feedback

<img width="1659" alt="BlinkR_Feedback" src="https://github.com/user-attachments/assets/51291e0b-3d6c-4803-a077-8b3c7305422d" />

Sends feedback to Google Drive.

> _Please feel free to share your feedback!_

---

## Admin Area

https://github.com/user-attachments/assets/fc510878-b4c8-4208-a7f1-cb37d908dce0

Only visible to users with "admin" role. Features include:

1. View all groups and permissions
2. Share/hide class protocol
3. Share/hide class data
4. Combine class data (creates a shared file in Google Drive; renames old files)
5. View report submissions
6. Access all group Google Drive folders

---

## 👩‍💻 Set Up and Deployment

A step-by-step guide is included for non-coders to set up and run BlinkR, including Google account integration and package installation. Follow the instructions in:

- `STEP0_install_packages.R`
- `STEP1_set_up_googledrive_script.R`
- `STEP2_define_variables.R`
- `STEP3_set_up_googledrive_script.R`

Once set up, you can run `app.R` to try it out or deploy using [shinyapps.io](https://www.shinyapps.io/).

---

## 💻 Code Structure

This app uses a **modular Shiny** architecture. Learn more about Shiny modules [here](https://shiny.posit.co/r/articles/improve/modules/).

---
