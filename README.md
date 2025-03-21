# Overview

BlinkR is a web app written in Shiny for R designed to help students plan, run, and analyse an experiment. It is intended to help them understand the fundamentals of experimental design and good research practices, as well as the basics for statistical analysis in R.

It runs entirely on **R** and instead of using more complex databases such as SQL, it is far more accessible with **google sheets and google drive** providing persistent storage.

Initially, the app was developed for an experiment that compared blinks per minute under stressed and control conditions, hence the name, however, changes have since been made allowing the different variables to be defined prior to starting, making it generalisable to any experiment that compares two groups.

**BlinkR can be used for experiments that compare two groups via T-test or Wilcoxon signed-rank test, depending on normality, with 3 technical replicates. In addition, BlinkR can handle paired and non-paired data.**

# 👁️ How to use BlinkR

You can view the demo app here: https://ds1405.shinyapps.io/blinkr_app/

## Google Drive

All persistent storage of this app runs on google drive, to make it easier to manage and more accessible. On log in, the unique 4-digit user ID is used to create a "session folder" within a google drive. This stores any text, code, analysis, or figures created within the app and can be viewed within BlinkR with the "View Google Drive" button. This serves two purposes: 1. backup of work carried out, 2. accessibility and ease of use. One key feature of BlinkR, is that is can be run on any device: computer, tablet, or smartphone. In cases where the computer is shared (e.g. computer lab or library) or where a mobile device is used, downloading can be difficult. As such, any data created within BlinkR is saved to a google drive and can be accessed at a later date for download.

The modal window that opens with the "View Google Drive" button also contains a dropdown selector and download button.

## Menu Bar
<img width="339" alt="BlinkR_Sidebar" src="https://github.com/user-attachments/assets/a8e12597-eff5-4830-87f4-95c224876990" />

https://github.com/user-attachments/assets/13a18c21-0278-4f52-93a3-c586577472e2

The full menu bar is available after authentication. At the top, the Group ID is displayed. This can be used for navigation and can be opened and closed using the "hamburger" icon in the top left. This also contains the log out button and a "View Google Drive" button. The latter opens a modal window that contains a link to the user's unique Google Drive folder (see above for more details).

## Introduction Log in

On opening the app you will get to the "Introduction" page which contains the log in block, prompting you to log in with an existing group ID or signing up with a new one.

### Log in
<img width="1565" alt="BlinkR_Intro_Log_in" src="https://github.com/user-attachments/assets/8c543237-a0a9-4a1e-ad3f-139e74f240c5" />

https://github.com/user-attachments/assets/f417d055-1070-40d7-a967-6505af0f9478

If you select "I have a Group ID", you are prompted for your 4-digit group ID. If you wish to test the admin functions you can log-in with .

### Sign up

If you have not signed up before, you can either enter your own 4-digit group ID and a name (or initial) or have a random number generated for you. This becomes your group ID with which you can log-in at later stages as well. If your group ID already exists in the user base you will receive an error, whereas the random number generator checks against the database to create a unique sequence.

Once log in/ sign up is completed, this block removes itself and you are free to explore the rest of the app.

### Introduction

https://github.com/user-attachments/assets/2e136476-471f-4321-8ebd-fd86aa053afc

Following authentication, the rest of this tab becomes visible to you. The "Research Roadmap" box contains shortcuts to the different tabs, with the prompted "Start Here" taking you to "Background".

## Background

This sections contains guides on how to begin background reading and research, including advice on how to start searching research databases e.g. PubMed.

There is no user input in this section.

## Hypotheses

https://github.com/user-attachments/assets/4f12a2b1-328d-4fed-82a9-9405f29a29a1

This sections contains guides on how to write a hypothesis and three user input boxes. These input boxes are designed to enter the hypothesis in plain language, the null hypothesis, and the alternative hypothesis. On pressing "Save Notes", each box is saved to the users Google Drive folder.

The content saved from these boxes is returned later in the app.


## Protocol

https://github.com/user-attachments/assets/c9f63872-bd88-43c8-9d2a-108fbf1f9538

This section is split into two main subsections: "Your Protocol" and "Class Protocol".

### Your Protocol

This tab contains multiple user input boxes for students to start brainstorming their ideas. The idea is that students can develop a protocol within their groups, which is then shared with the admin/ educator via a google sheet, who then combines the protocol for the whole class. Alternatively, this can be used to get students to think about a protocol, regardless if this is then used for the class protocol.

### Class Protocol

This section can be shared/ hidden to specific groups by the admin. This can be either done in the admin section (see below for more information) or directly in the user base by setting "Protocol" to TRUE. This populates the "Class Protocol" tab.

### Data Hazards

Below the protocol box, there is an additional box prompting students to consider the data hazards associated with this project (learn more [here](https://datahazards.com/index.html)). Each hazard can be dragged from the left to the right "Selected Hazards" box, as they deem appropriate. Below this drag-and-drop feature is a "Submit" button, that upload a pdf to the user's Google Drive folder containing the selected hazards. **This can be overwritten.**

## Measurements

https://github.com/user-attachments/assets/a0868a4b-0ed0-4283-b61a-6d0b7260fb63

This section allows students to record the measurements of their experiment.

The first step allows subjects/ participants to be added, which generates a unique ID.

For each participant added, a new "Measurements" box is added to the UI, each with the unique ID and the entered initials. This can be deleted with the "Delete Student" button.

https://github.com/user-attachments/assets/68f81347-07e1-4b34-8ff2-e058aa6e8857

Each Measurements box contains three tabs:

1. **Consent Agreement**
2. **Level 1 Measurement (defined variable)**
3. **Level 2 Measurement (defined variable)**

---

1. **Consent Agreement**
   Participants must read and agree to the consent statement. **The "Submit" button on the measurements tabs is conditional to the consent box being ticked.**

2. **Level 1 - Measurements** and **Level 2 - Measurements**
   The title of these tabs depends on the variable defined for the levels (see "Set up"), making it dynamic to the specific experiment you are running. Both or either of the tabs can be completed, depending on the experimental design (both for paired design). On "Submit", the data entered into these fields is stored in a google sheet and returned in the "Raw Data" section. These fields can be overwritten and on re-submission you will get a warning that you are about to overwrite.

This area is designed to take three technical replicates.

## Raw Data

<img width="1659" alt="BlinkR_Raw_Data" src="https://github.com/user-attachments/assets/05c5f92e-eb3a-4666-af3b-19059464015f" />

Similar to the "Protocol" tab, this contains two sections: "Your Group Data" and "Class Data".


### Your Group Data

This is a dynamic data table that is populated by the entries from the measurements tab. Users can only see data for their own group here.

### Class Data

This is a view of the combined class data. Permission to view this can be set in the "Admin" section (see below) or directly in the user base by setting "Data" to TRUE.

## Playground

https://github.com/user-attachments/assets/47222550-d012-457c-87c5-0d364be0acd7

This section is the first introduction to the R coding elements throughout the analysis section.

These elements are made up of:

1. **Code Editor:** Virtual R code editor
2. **Send Code to Editor:** This sends a predefined code (usually what is displayed in the instructions) to the editor
3. **Run Code:** Runs the code with a spinner
4. **Clear Code:** Removes any code entered into the editor (and removes the console output)
5. **Dynamic Console:** Prints the output of the editor

In this section, students are encouraged to try some basic commands to understand the editor.

## Analysis Dashboard

This section is the beginning of the analysis section. Initially, it contains buttons to the different menu items (e.g. "Prepare Data" or "Statistical Analysis), but with use it becomes populated with results. It leads the user through the section step-by-step.

<img width="1659" alt="BlinkR_Analysis_Dashboard_empty" src="https://github.com/user-attachments/assets/320ab7e5-0282-4136-935a-e2f6a3a22040" />

## Prepare Data

https://github.com/user-attachments/assets/87fc2f8e-c873-41c3-acf8-133b32a4c8be

This section contains two elements to view the data and to prepare the data (average the technical replicates).

### View Data

This is a simple beginning, where the data is viewed with `head(data)`. On correct output, feedback is generated which also contains text input boxes to check understanding and keep users engaged.

### Prepare Data

This section uses `dplyr`to calculate the averages of the technical replicates for each ID and "treatment group"/ level. The variables for the code and the text explanation are also pre-defined (see "Set up").

## Summarise the Data

https://github.com/user-attachments/assets/98c20120-6558-4f1f-8834-da44c65b914d

This section walks users through generating summary statistics (mean, sd, n, and sem). This is done slowly, first by showing them how to do this for one group (suggested: control group - this is defined in the "Set up"). This is then followed by "Your turn" where users have to the code they just learned for the second group. Each element is dependent on the successful completion of the previous step. In the final step, user are shown how to generate this quickly with `dplyr`, with a quiz checking understanding.

The final summary step with `dplyr`also contains the button "Save to Dashboard", which saves this result to the analysis dashboard and uploads to the google drive.

https://github.com/user-attachments/assets/4c51e0a9-fb91-40e1-bc18-c9dc0752e092

## Create a Figure

https://github.com/user-attachments/assets/d5daa338-c34f-4bdb-988f-f263cba9103d

Here, users have the option of creating a bar chart or a box plot (both with scatter). Upon creation, they are encouraged to check the plots and enter a title and axis-labels, as well as change the colours of the plot. When finished, this should also be saved to the "Analysis Dashboard" with the "Save to Analysis Dashboard" button, which also uploads the results to the Google Drive folder. As before, the variables are dependent on "Set up".

## Statistical Analysis

https://github.com/user-attachments/assets/a8d35939-f1e9-4dbe-8364-5db8f9013692

The first step here is to check for normality. A histogram is generated dynamically from the given class data and printed to the user with the "Generate Histogram to Check for Normality" button. Here too, the variables are dependent on "Set up".

The flow is as follows:

1. **Generate Histogram** - prints the histogram
2. **Decide if normal or not normal** - "Normal" selection initiates the T-Test flow, "Not Normal" initiates the Wilcoxon signed-rank flow
3. **Decide if paired or not paired** - this determines the code for the respective statistical tests
4. **Hypothesis testing** - depending on what was selected this will either return
   - Normal Unpaired: T-Test
   - Normal Paired: Paired T-Test
   - Not Normal Unpaired: Wilcoxon signed-rank
   - Not Normal Paired: Paired Wilcoxon signed rank
5. **Effect size** - depending on the hypothesis test carried out this, the appropriate effect size analysis code is made available. **THIS ONLY APPEARS IF p≤0.05**
6. **Interpretation** - user input boxes asking for p-value (and effect size if p≤0.05), returning the hypothesis entered in the "Hypothesis" section, a radio button quiz asking if H0 or HA is correct (feedback depends on p≤0.05), and a text box to enter a one sentence interpretation of the results

As before, the "Save to Dashboard" button saves the results and uploads them to the Google Drive folder.

## Writing Up Advice

<img width="1659" alt="BlinkR_Writing_Advice" src="https://github.com/user-attachments/assets/316bcb1e-5490-43ac-bd05-761c17129238" />

This section contains guidance and advice on how to write up this experiment. In addition, there is further guidance on how to work together as a group and general tips and tricks.

There is no user input here.

## AI

<img width="1659" alt="BlinkR_AI" src="https://github.com/user-attachments/assets/984defd0-7b9f-46a2-bd66-77ca27c700fa" />

This sections contains guidance on what AI can and cannot be used for in writing up this report, stressing potential consequences (e.g. academic misconduct).

There is no user input here.

## Write Up

https://github.com/user-attachments/assets/0b4562b1-55ec-4af6-9a4c-8e4cea3a95b1

This sections contains user input boxes to allow them to write notes and outline their report. All notes saved here are uploaded to the Google Drive folder. **They cannot be overwritten.**

## Upload Final Report

<img width="1659" alt="BlinkR_Upload" src="https://github.com/user-attachments/assets/605190da-4167-4415-bb99-4a54c6b6c441" />

This is an optional space for users to upload their final reports. It gets saved using the group ID and date + time. The reports are uploaded to the Google Drive and can also be monitored in the "Admin" section.

## Simulated Experiment

In order to provide more context for the practical experiment, this section contains a simulated experiment with "dummy data", walking them through each step with a concrete example. This also includes a final write-up and critique of an AI-Generated report. **Anything generated here is not saved to the Google Drive.**

## Feedback
<img width="1659" alt="BlinkR_Feedback" src="https://github.com/user-attachments/assets/51291e0b-3d6c-4803-a077-8b3c7305422d" />

This final section sends feedback to the Google Drive.

**Please feel free to share this feedback with me!!**

## 21 Admin Area

https://github.com/user-attachments/assets/fc510878-b4c8-4208-a7f1-cb37d908dce0

This area is only visible to those with Admin roles. This can be set in the user base by setting Role to "admin".

In this area, the admin can

1. **View All Groups**: all groups and permissions
2. **Share Class Protocol**: share or hide the class protocol to selected or all groups (can also be selected by date)
3. **Share Class Data**: share or hide the class data
4. **Combine Class Data**: This creates the "Shared Class Data" file in the Google Drive, combining each group's measurement result
5. **View Report Submission**
6. **View Google Drive**: this instance of this button allows the admin to view all groups' Google Drive folders.

---

# 👩‍💻 Set up and deployment

Even if you do not know how to code, I will break down the set up and deployment of this web app so you can use it with your own class!

## 22 Download or Clone this Repo

## 23 Download R

## 24 Google Drive and Google Sheets

# 💻 Code Strucutre Information

## 25 Modular Approach

## 26 Ace Editor Module

# ➡️ PRs
