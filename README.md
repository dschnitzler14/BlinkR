
# BlinkR: A Shiny App for Exploring Blinking as a Stress Response

Empowering undergraduates to engage in the scientific process through interactive class experiments.

## 1	Introduction
BlinkTrackR is an interactive Shiny app designed for undergraduate students to conduct and analyse a class experiment examining blinking as a physiological response to stress. The app guides users through every step of the scientific process in a scaffolded approach —from researching background information to formulating hypotheses, designing protocols, collecting and entering data, performing statistical analysis in R, creating visualisations, interpreting results, and writing up conclusions. 

Currently, this app is tailored specifically for use within MSB, but the long-term goal is to make it accessible to educators and students across different institutes. This Shiny app can run entirely on mobile, thereby increasing accessibility for all students and educators. 

## 2	Structure
This app is modular:

- `app.R`:
	- core file required for running
	- automatically loads all modules from the `/modules` folder.
	- handles the structure (e.g., header, sidebar, and navigation).
- `data`: 
	- contains dummy class data for testing
- `modules`:
	- Includes all modular components, with each tab as a separate module.
	- Some modules serve as shared components that are loaded into other modules.
- `markdown`:
	- each sub-folder contains large text content that is loaded into the modules/ app.R as required
- `www`:
	- css styling

## 3	Modifying the App
- for testing purposes, group log in is
	- Group: user1
	- password: pass1
### 3.1	Adding a New Tab

1. Create a new module file in the `/modules` folder.
2. Define your module using the `moduleServer` and `moduleUI` patterns.
3. The new module will automatically be loaded into the app without manual registration in `app.R`.

### 3.2	Editing Large Text Content

- Modify the relevant markdown files in the `markdown/` directory.
- Changes are automatically reflected in the app (if the `.md` file already being called)

### 3.3	Customizing Styles

- Edit the CSS files in the `www/` folder to change the app's appearance.

