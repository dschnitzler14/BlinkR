---
title: "BlinkR: An Accessible, Interactive Tool for Teaching Experimental Design and Data Analysis"
tags:
  - R
  - shiny
  - statistics
  - scientific education
  - higher education
  - multi-lingual
authors:
  - name: Daniela Schnitzler
    orcid: 0000-0002-4189-2688
    affiliation: 1
  - name: Melanie I. Stefan
    orcid: 0000-0002-6086-7357
    affiliation: 1
affiliations:
  - name: Institute of Neuroscience and Biopsychology for Clinical Application, Medical School Berlin, Germany
    index: 1

date: 18 September 2025
bibliography: paper.bib
header-includes:
  - \usepackage{longtable}
  - \usepackage{tabularx}
  - \usepackage{booktabs}
---

# Statement of Need

Undergraduate students in medical and medical research programmes often struggle to engage with data analysis due to limited coding experience, lack of appropriate devices, and language barriers [@paciFacingFearScaffolding2025; @williamsBarriersIntegrationBioinformatics2019]. To address these challenges, BlinkR was developed: an interactive web app designed to support the teaching of scientific practices using R in an accessible, mobile-friendly, and multilingual format.

BlinkR was initially intended to support a specific course on the fundamentals of research delivered to first-year medical students by the authors through a research-informed approach to learning and teaching [@griffithsKnowledgeProductionResearch2004; @hagtvedtPedagogicalSimulationSampling2007]. The aim of the course for first-year undergraduates is to understand the process and practice of science, including designing an experiment, carrying out the measurements, analysing the results with R, and writing up a final report.

BlinkR is an interactive web app that guides students through all steps of this process, using explanations, writing and coding prompts, quizzes and interactive feedback. The interactive web app format was chosen based on the following practical assumptions:

1. Most students at this level in a medical or medical research degree programme have little to no experience writing R code (or any code).
2. Students typically attend classes and tutorials with mobile phones or tablets, rather than laptops.
3. Downloading and installing R and an IDE during class time is time-consuming and would reduce teaching time. Students are unlikely to complete these steps before class.
4. Providing instruction in the students' teaching language (German) promotes greater understanding and engagement compared to English-only materials.

BlinkR was designed not only to make learning more engaging, but also to foster student agency, support exploration through immediate feedback and validation, build confidence in scientific thinking, and provide meaningful context for the research process. Importantly, it offers an accessible entry point to R and data analysis for students who might otherwise be excluded due to technical or linguistic barriers. While the app was developed for undergraduates, it may be suitable for secondary school students as well.

The original experiment embedded in the app examined whether watching a stressful video affects blinking rates compared to watching a neutral video. This study design also inspired the name "BlinkR".

# Software Description

BlinkR is a web application built using the *Shiny* framework for R [@changShinyWebApplication2025], with Google Drive serving as the backend for persistent data storage. This deliberate choice enhances accessibility for educators who may not be familiar with more complex database systems like SQL. R was selected as the development language due to its wide adoption in life sciences and education, as well as the extensive support available for _Shiny_-based projects [@paciFacingFearScaffolding2025; @wangStudentDevelopedShinyApplications2021]. This makes the app more approachable and adaptable for educators with narrow technical expertise.

Before deployment, educators can define the experimental variables, enabling BlinkR to support a range of simple two-group experiments with three technical replicates for each group ("group A" and "group B"). These can be either a paired or unpaired design, and the results can be analysed by a parametric t-test or non-parametric Wilcoxon test, depending on the normality of the distribution.

The data is loaded dynamically through Google Drive, ensuring that quiz answers and feedback responses are adaptive and specific to the supplied data.

Briefly, BlinkR contains a structured workflow guiding students through the experimental process, however, each section can also be carried out in isolation, independent of previous sections. The overview of the app is as follows:

- **Introduction:** Overview of the BlinkR app and usage
- **Background:** Guidance on using research databases and how to begin a literature search relevant to the experiment
- **Hypothesis:** Formulate null and alternative hypotheses
- **Protocol:** Plan the experiment; view "group protocol ideas" and, if shared by the admin, "class protocol"
- **Measurements:** Students enter measurements for both conditions. Data is saved dynamically to Google Drive
- **Raw Data:** View raw individual, group, and class-level data (if shared)
- **Playground:** Introduction to R syntax and working with R code to prepare for analysis
- **Analysis Dashboard:** Overview of all analyses; includes link to the group's Google Drive folder
  - **1. Prepare Data:** Students review data, average replicates, and complete a quiz to confirm understanding
  - **2. Summarise Data:** Perform summary statistics, calculate mean, SD, SEM, and create summary table using `dplyr`
  - **3. Create Figure:** Create box plot or bar chart. Students label and style plots manually
  - **4. Statistical Analysis:** Assess normality, choose statistical test (paired/unpaired), and calculate effect size if p≤ 0.05
- **Write Up Advice:** Guidance on scientific writing and group collaboration
- **AI:** Responsible use of generative AI tools, with a warning about academic misconduct
- **Upload Final Report:** Submit the final report to Google Drive
- **Simulated Experiment:** Run through a complete example using dummy data
- **Feedback:** Students provide feedback that is saved to Google Drive

Additional UI features include login/logout functionality, a direct view of the connected Google Drive, as well as a language selector (English or German). Educators (or administrators) can manage users, control data visibility (e.g., class protocols or shared datasets), either through the "Users" spreadsheet on Google Drive or via the dedicated Admin Area. Within this section, they can also generate new groups and monitor report submissions.

The [source code](https://github.com/dschnitzler14/BlinkR.git) is openly available on GitHub. A detailed README provides full setup instructions, including cloning the repository, adapting variables, configuring Google Drive integration, and running the necessary setup scripts for educators.

# Acknowledgements

This software was developed with the support of the Stefan Lab at Medical School Berlin (MSB). I would also like to thank Dr Patricia Rubisch and Ms Kira Lampert, as well as the students of the winter 2024/25 and summer 2025 terms M14 class at MSB for beta-testing and providing feedback.

# References
