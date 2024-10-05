REM
REM Unzip file
REM

If WScript.Arguments.Count = 0 Then
    WScript.Echo "Usage: " & WScript.ScriptName & " ZipPath [ExtractPath]"
    WScript.Quit 1
End If

Set Fs = CreateObject("Scripting.FileSystemObject")

ZipPath = WScript.Arguments(0)
If NOT Fs.FileExists(ZipPath) Then
    WScript.Echo "File Not Found: " & ZipPath
    WScript.Quit 1
Else
    ZipPath = Fs.GetAbsolutePathName(ZipPath)
End If

If WScript.Arguments.Count = 1 Then
    Set File = Fs.GetFile(ZipPath)
    ExtractPath = Fs.GetParentFolderName(File.Path)
Else
    ExtractPath = WScript.Arguments(1)
End If

If NOT Fs.FolderExists(ExtractPath) Then
    Fs.CreateFolder(ExtractPath)
    ExtractPath = Fs.GetAbsolutePathName(ExtractPath)
End If

REM WScript.Echo ZipPath & " -> " & ExtractPath

Set Shell = CreateObject("Shell.Application")
Set Files = Shell.NameSpace(ZipPath).items
NoProgress = 4
Shell.NameSpace(ExtractPath).CopyHere Files, NoProgress
