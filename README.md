# qPCR_app
Shiny App for analysis and visualization of qPCR results from the output files of the ImageQuant3 qPCR machine.

The Premium Plot feature allows you to plot the gene expression of your samples together with the experimental conditions of your experiment. To do so, you need to upload an Experimental Design Table in tidy format (one column = one variable). 

IMPORTANT: There must be a column named "Sample" and it should be writen with a capitalized "S" and the name of your samples should be the same as in the qPCR machine output files. Here there is an example of how this table should look like: 

| Sample | ecotype   | copper | tissue |   |
|--------|-----------|--------|--------|---|
| c19    | barcelona | 0.1uM  | ROOTS  |   |
| c20    | barcelona | 0.1uM  | ROOTS  |   |
| c21    | barcelona | 0.1uM  | ROOTS  |   |
| c22    | barcelona | 1uM    | ROOTS  |   |
| c23    | barcelona | 1uM    | ROOTS  |   |
| c24    | barcelona | 1uM    | ROOTS  |   |
| c25    | barcelona | 20uM   | ROOTS  |   |
| c26    | barcelona | 20uM   | ROOTS  |   |
| c27    | barcelona | 20uM   | ROOTS  |   |
| c28    | amsterdam | 0.1uM  | ROOTS  |   |
| c29    | amsterdam | 0.1uM  | ROOTS  |   |
| c30    | amsterdam | 0.1uM  | ROOTS  |   |
| c31    | amsterdam | 1uM    | ROOTS  |   |
| c32    | amsterdam | 1uM    | ROOTS  |   |
| c33    | amsterdam | 1uM    | ROOTS  |   |
| c41    | amsterdam | 20uM   | ROOTS  |   |
| c42    | amsterdam | 20uM   | ROOTS  |   |
| c44    | amsterdam | 20uM   | ROOTS  |   |