REM
REM Run VeLisp script without showing the Command Prompt
REM Useful for creating Shortcuts
REM

If (WScript.Arguments.Count = 0) Then
    WScript.Echo "Usage: " & WScript.ScriptName & " VeLispScript [Arg...]"
    WScript.Quit 1
End If

ScriptPath = Left(WScript.ScriptFullName, Len(WScript.ScriptFullName) - Len(WScript.ScriptName) - 1)

REM Windows 10 doesn't allow starting unknown programs in
REM the hidden state. We trick it here by first running
REM something it knows really well: cmd.exe /C.

ReDim Init(2)
Init(0) = "cmd.exe /C"
Init(1) = ScriptPath & "\" & "velisp.exe"

ReDim Args(WScript.Arguments.Count + 1)

For i = 0 To WScript.Arguments.Count - 1
   Args(i) = """" & WScript.Arguments(i) & """"
Next

Set WshShell = WScript.CreateObject("WScript.Shell")
WshShell.Run Join(Init) & Join(Args), 0, False
