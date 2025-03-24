# international-BRA-EUR-2025
Workspace for 2025 edition of BRA-EUR comparison

This repository, i.e., BRA-EUR-2025, supports the development of the 2024 BRA-EUR report. The report is established as a quarto book.

Things to note/recall.

The sequence of "chapters" (and layout options) is controlled via _quarto.yml. 
When adding a new chapter, make sure to include the reference to the chapter in the .yml file!

Changes to the qmd/chapter file are affecting the "source". 
To include these into the book output, the book needs to be built: i.e. go to the Build pane (top right, next to git pane) and render the book. 
Alternatively, use the command quarto render in the terminal (bottom, next to console)

During the development, we do not push the locally built book files. 
This is now changed in .gitignore. 
If the rendering the book throws an error, delete the _book folder. 
Rendering the book locally will regenerate all the output files (and this folder). 
This avoids nasty sync errors between development version on our laptops.
We upload newer versions as the draft matures and then start on finalising the work.
