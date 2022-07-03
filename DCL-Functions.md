### Table of contents

- [Application-Specific Data-Handling Functions](#application-specific-data-handling-functions)
- [Dialog Box Opening and Closing Functions](#dialog-box-opening-and-closing-functions)
- [Image Tile-Handling Functions](#image-tile-handling-functions)
- [List Box and Pop-Up List-Handling Functions](#list-box-and-pop-up-list-handling-functions)
- [Tile and Attribute-Handling Functions](#tile-and-attribute-handling-functions)

### Application-Specific Data-Handling Functions

| Function | Description | Info |
|----------|-------------|------|
| (client_data_tile key clientdata) | Associates application-managed data with a dialog box tile | |

### Dialog Box Opening and Closing Functions

| Function | Description | Info |
|----------|-------------|------|
| (done_dialog [status]) | Terminates a dialog box | |
| (load_dialog dclfile) | Loads a DCL file | |
| (new_dialog dlgname dcl_id [action [screen-pt]]) | Begins a new dialog box and displays it, and can also specify a default action | |
| (start_dialog) | Displays a dialog box and begins accepting user input | |
| (term_dialog) | Terminates all current dialog boxes as if the user cancels each of them | |
| (unload_dialog dcl_id) | Unloads a DCL file | |


### Image Tile-Handling Functions

| Function | Description | Info |
|----------|-------------|------|
| (dimx_tile key) | Retrieves the width of a tile in dialog box units | |
| (dimy_tile key) | Retrieves the height of a tile in dialog box units | |
| (end_image) | Ends creation of the currently active dialog box image | |
| (fill_image x1 y1 wid hgt color) | Draws a filled rectangle in the currently active dialog box image tile | |
| (slide_image x1 y1 wid hgt sldname) | Displays an AutoCAD slide in the currently active dialog box image tile | Expected in vTBD |
| (start_image key) | Starts the creation of an image in the dialog box tile | |
| (vector_image x1 y1 x2 y2 color) | Draws a vector in the currently active dialog box image | |

### List Box and Pop-Up List-Handling Functions

| Function | Description | Info |
|----------|-------------|------|
| (add_list string) | Adds or modifies a string in the currently active dialog box list | |
| (end_list) | Ends processing of the currently active dialog box list | |
| (start_list key [operation [index]]) | Starts the processing of a list in the list box or in the pop-up list dialog box tile | |

### Tile and Attribute-Handling Functions

| Function | Description | Info |
|----------|-------------|------|
| (action_tile key action-expression) | Assigns an action to evaluate when the user selects the specified tile in a dialog box | |
| (get_attr key attribute) | Retrieves the DCL value of a dialog box attribute | |
| (get_tile key) | Retrieves the current runtime value of a dialog box tile | |
| (mode_tile key mode) | Sets the mode of a dialog box tile | |
| (set_tile key value) | Sets the value of a dialog box tile | |

DCL Functions from https://help.autodesk.com/ are used in accordance with https://creativecommons.org/licenses/by-nc-sa/3.0/
