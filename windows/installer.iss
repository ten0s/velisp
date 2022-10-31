#define Version "{{version}}"

[Setup]
AppName=VeLisp
AppVersion={#Version}
WizardStyle=modern
ArchitecturesAllowed=x64
ArchitecturesInstallIn64BitMode=x64
DefaultDirName={autopf}\VeLisp
DefaultGroupName=VeLisp
UninstallDisplayIcon={app}\velisp.exe
Compression=lzma2
SolidCompression=yes

[Files]
Source: "velisp-{#Version}-win-x64\*"; DestDir: "{app}"; Flags: recursesubdirs ignoreversion

[Icons]
Name: "{group}\VeLisp";                  Filename: "{app}\velisp.exe"; WorkingDir: "{app}"
Name: "{group}\VeLisp Examples\Calc";    Filename: "{app}\noprompt.vbs"; Parameters: """{app}\velisp.exe"" ""{app}\examples\calc.lsp""";    WorkingDir: "{app}";
Name: "{group}\VeLisp Examples\Demo";    Filename: "{app}\noprompt.vbs"; Parameters: """{app}\velisp.exe"" ""{app}\examples\demo.lsp""";    WorkingDir: "{app}";
Name: "{group}\VeLisp Examples\Fifteen"; Filename: "{app}\noprompt.vbs"; Parameters: """{app}\velisp.exe"" ""{app}\examples\fifteen.lsp"""; WorkingDir: "{app}";
Name: "{group}\VeLisp Examples\Mines";   Filename: "{app}\noprompt.vbs"; Parameters: """{app}\velisp.exe"" ""{app}\examples\mines.lsp""";   WorkingDir: "{app}";
Name: "{group}\VeLisp Website";          Filename: "https://github.com/ten0s/velisp"
Name: "{group}\Uninstall VeLisp";        Filename: "{uninstallexe}"
