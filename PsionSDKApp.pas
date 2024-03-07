{$mode objfpc}{$H+}{$J-}
unit PsionSDKApp;

interface
uses sysutils;

type
    TPsionSDKParam = record
        Exists : Boolean;
        Value : string;
    end;

    TPsionSDKParamList = array[1..36] of TPsionSDKParam;

    TPsionSDKAppParams = class
        // TODO: Add allowed params (switch, description, optional, type?)
        // TODO: Check that param exists
        // TODO: Seek param and get value
        strict private
            _Filename : string;
            _ParamList : TPsionSDKParamList;
            function _EncodeParamID(s : String) : Integer;
            function _DecodeParamID(id : Integer) : String;
        public
            constructor Create();
            procedure Grab();
            function SwitchExists(sw: String) : Boolean;
            function InSwitch(sw: String; val: String) : Boolean;
            function SwitchVal(sw: String) : String;
            property Filename : string read _Filename;
            property Params : TPsionSDKParamList read _ParamList;
    end;
implementation

constructor TPsionSDKAppParams.Create();
begin
    inherited Create;
end;

function TPsionSDKAppParams._EncodeParamID(s : String) : Integer;
begin
    case Ord(s[1]) of
        65..90:  Result := Ord(s[1]) - 64;
        97..122: Result := Ord(s[1]) - 96;
        48..57:  Result := Ord(s[1]) - 21;
        else     Result := 0;
    end;
end;

function TPsionSDKAppParams._DecodeParamID(id : Integer) : String;
begin
    case id of
        1..26:  Result := Chr(id + 64);
        27..36: Result := Chr(id + 21);
        else    Result := '';
    end;
end;

function TPsionSDKAppParams.SwitchExists(sw: String) : Boolean;
begin
    Result := _ParamList[_EncodeParamID(LeftStr(sw, 1))].Exists;
end;

function TPsionSDKAppParams.InSwitch(sw: String; val: String) : Boolean;
begin
    Result := (_ParamList[_EncodeParamID(LeftStr(sw, 1))].Exists and (Pos(LeftStr(val, 1), UpCase(_ParamList[_EncodeParamID(LeftStr(sw, 1))].Value)) > 0));
end;

function TPsionSDKAppParams.SwitchVal(sw: String) : String;
begin
    Result := _ParamList[_EncodeParamID(LeftStr(sw, 1))].Value;
end;


procedure TPsionSDKAppParams.Grab();
var
    cur, tot : Integer;
    thisParam : String;
    flgFoundName: Boolean = false;
    param : TPsionSDKParam;
    switch : String[1];
    switch_id : Integer;
begin
    tot := paramCount();
    cur := 1;

    for cur := 1 to tot do
    begin
        thisParam := paramStr(cur);
        if (thisParam[1] = '-') and (length(thisParam) > 1) then begin
            switch_id := _EncodeParamID(thisParam[2]);
            if switch_id > 0 then begin
                if _ParamList[switch_id].Exists then begin
                    WriteLn('Switch ''', UpCase(thisParam[2]), ''' already used.');
                    halt;
                end;

                _ParamList[switch_id].Exists := true;
                if length(thisParam) > 2 then begin
                    _ParamList[switch_id].Value := copy(thisParam, 3);
                end;
                WriteLn('Switch ''', _DecodeParamID(switch_id), ''' (', switch_id, ') value ''', _ParamList[switch_id].Value, '''');
            end;
        end else begin
            if flgFoundName then begin
                WriteLn('Too many items without a switch.');
                halt;
            end;
            writeln('Found name of file to process: ', thisParam);
            flgFoundName := true;
            _Filename := thisParam;
        end;
    end;
end;

end.
