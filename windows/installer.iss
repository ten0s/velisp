#define Version "{{version}}"

[Setup]
AppName=VeLisp
AppId=ten0s/velisp
AppVersion={#Version}
AppPublisher=Dmitry Klionsky
AppPublisherURL=https://github.com/ten0s/velisp
AppSupportURL=https://github.com/ten0s/velisp
AppUpdatesURL=https://github.com/ten0s/velisp
VersionInfoVersion={#Version}
VersionInfoCompany=Dmitry Klionsky
VersionInfoCopyright=GNU General Public License v3
VersionInfoDescription=AutoLISP interpreter with DCL support
LicenseFile=LICENSE
ArchitecturesAllowed=x64
ArchitecturesInstallIn64BitMode=x64
DefaultDirName={autopf}\VeLisp
DefaultGroupName=VeLisp
Compression=lzma2
SolidCompression=yes
UninstallDisplayIcon={app}\velisp.exe
WizardStyle=modern
ChangesEnvironment=true

[Files]
Source: "velisp-{#Version}-win-x64\*"; DestDir: "{app}"; Flags: recursesubdirs ignoreversion

[Dirs]
Name: "{app}\shortcuts"

[Icons]
Name: "{group}\VeLisp";                  Filename: "{app}\velisp.exe"; WorkingDir: "{app}"
Name: "{group}\VeLisp Command Prompt";   Filename: "{cmd}"; Parameters: "/k"; WorkingDir: "{app}"
Name: "{group}\VeLisp Website";          Filename: "https://github.com/ten0s/velisp"
Name: "{group}\Examples";                Filename: "{app}\shortcuts"
Name: "{app}\shortcuts\Source Code";     Filename: "{app}\examples"
Name: "{app}\shortcuts\Calc Example";    Filename: "{app}\noprompt.vbs"; Parameters: """{app}\velisp.exe"" ""{app}\examples\calc.lsp""";    WorkingDir: "{app}"; IconFilename: "{app}\velisp.exe"
Name: "{app}\shortcuts\Demo Example";    Filename: "{app}\noprompt.vbs"; Parameters: """{app}\velisp.exe"" ""{app}\examples\demo.lsp""";    WorkingDir: "{app}"; IconFilename: "{app}\velisp.exe"
Name: "{app}\shortcuts\Fifteen Example"; Filename: "{app}\noprompt.vbs"; Parameters: """{app}\velisp.exe"" ""{app}\examples\fifteen.lsp"""; WorkingDir: "{app}"; IconFilename: "{app}\velisp.exe"
Name: "{app}\shortcuts\Mines Example";   Filename: "{app}\noprompt.vbs"; Parameters: """{app}\velisp.exe"" ""{app}\examples\mines.lsp""";   WorkingDir: "{app}"; IconFilename: "{app}\velisp.exe"
Name: "{group}\Uninstall";               Filename: "{uninstallexe}"

[Tasks]
Name: envPath; Description: "Add to PATH variable"

#include "windows/installer-libpath.iss"
[Code]
procedure CurStepChanged(CurStep: TSetupStep);
begin
    if (CurStep = ssPostInstall) and WizardIsTaskSelected('envPath')
    then EnvAddPath(ExpandConstant('{app}'));
end;

procedure CurUninstallStepChanged(CurUninstallStep: TUninstallStep);
begin
    if CurUninstallStep = usPostUninstall
    then EnvRemovePath(ExpandConstant('{app}'));
end;
