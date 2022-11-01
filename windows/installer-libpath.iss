[Code]
function RegRootKey(): Integer;
begin
    if IsAdmin() then
        Result := HKEY_LOCAL_MACHINE
    else
        Result := HKEY_CURRENT_USER;
end;

function RegKeyName(): String;
begin
    if IsAdmin() then
        Result := 'SYSTEM\CurrentControlSet\Control\Session Manager\Environment'
    else
        Result := 'Environment';
end;

{ Adjusted from https://stackoverflow.com/a/46609047/587698 }
procedure EnvAddPath(Path: String);
var
    RootKey: Integer;
    KeyName: String;
    Paths: string;
begin
    RootKey := RegRootKey();
    KeyName := RegKeyName();

    { Retrieve current path (use empty string if entry not exists) }
    if not RegQueryStringValue(RootKey, KeyName, 'Path', Paths)
    then Paths := '';

    { Skip if string already found in path }
    if Pos(';' + Uppercase(Path) + ';', ';' + Uppercase(Paths) + ';') > 0 then exit;

    { App string to the end of the path variable }
    Paths := Paths + ';'+ Path +';'

    { Overwrite (or create if missing) path environment variable }
    if RegWriteStringValue(RootKey, KeyName, 'Path', Paths)
    then Log(Format('The [%s] added to PATH: [%s]', [Path, Paths]))
    else Log(Format('Error while adding the [%s] to PATH: [%s]', [Path, Paths]));
end;

{ Adjusted from https://stackoverflow.com/a/46609047/587698 }
procedure EnvRemovePath(Path: String);
var
    RootKey: Integer;
    KeyName: String;
    Paths: String;
    P: Integer;
begin
    RootKey := RegRootKey();
    KeyName := RegKeyName();

    { Skip if registry entry not exists }
    if not RegQueryStringValue(RootKey, KeyName, 'Path', Paths) then
        exit;

    { Skip if string not found in path }
    P := Pos(';' + Uppercase(Path) + ';', ';' + Uppercase(Paths) + ';');
    if P = 0 then exit;

    { Update path variable }
    Delete(Paths, P - 1, Length(Path) + 1);

    { Overwrite path environment variable }
    if RegWriteStringValue(RootKey, KeyName, 'Path', Paths)
    then Log(Format('The [%s] removed from PATH: [%s]', [Path, Paths]))
    else Log(Format('Error while removing the [%s] from PATH: [%s]', [Path, Paths]));
end;
