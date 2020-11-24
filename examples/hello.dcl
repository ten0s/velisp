// Example from https://en.wikipedia.org/wiki/Dialog_Control_Language

name_button : button {
 label = "Submit name";
 action = "(change-name)"; 
}

hello : dialog {
 label = "DCL Example";
 : edit_box {
   label = "Name: ";
   key = "name";
 }
 : name_button {
   key = "submit-name";
 }
 : text {
   key = "greeting";
 }
 ok_only;
}
