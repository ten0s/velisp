REM
REM Run program without showing the Command Prompt
REM Useful for creating Shortcuts
REM

If (WScript.Arguments.Count = 0) Then
    WScript.Echo "Usage: " & WScript.ScriptName & " Prog [Arg...]"
    WScript.Quit 1
End If

REM Windows 10 does NOT allow starting unknown programs in
REM the hidden state. We trick it here by first running
REM something it knows really well: cmd.exe /C.

ReDim Cmd(2)
Cmd(0) = "cmd.exe"
Cmd(1) = "/C"

ReDim Args(WScript.Arguments.Count)
For i = 0 To WScript.Arguments.Count - 1
   Args(i) = """" & WScript.Arguments(i) & """"
Next

Command = Join(Cmd) & """" & Join(Args) & """"
REM WScript.Echo Command

Set WshShell = WScript.CreateObject("WScript.Shell")
HideWindow = 0
WaitOnReturn = True
WshShell.Run Command, HideWindow, WaitOnReturn
