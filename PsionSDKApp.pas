{$mode objfpc}{$H+}{$J-}
unit PsionSDKApp;

interface
uses sysutils;

type
    TPsionSDKParam = record
        Switch : string[1];
        Value : string;
    end;

    TPsionSDKParamList = array of TPsionSDKParam;

    TPsionSDKAppParams = class
        // TODO: Add allowed params (switch, description, optional, type?)
        // TODO: Check that param exists
        // TODO: Seek param and get value
        strict private
            _Filename : string;
            _ParamList : TPsionSDKParamList;
        public
            constructor Create();
            procedure Grab();
            property Filename : string read _Filename;
            property Params : TPsionSDKParamList read _ParamList;
    end;
implementation

constructor TPsionSDKAppParams.Create();
begin
    inherited Create;
end;

procedure TPsionSDKAppParams.Grab();
var
    cur, tot : Integer;
    thisParam : String;
    flgFoundName: Boolean = false;
    param : TPsionSDKParam;
begin
    tot := paramCount();
    cur := 1;

    for cur := 1 to tot do
    begin
        thisParam := paramStr(cur);
        if thisParam[1] = '-' then begin
            if length(thisParam) > 1 then begin
                param.Switch := copy(thisParam, 2, 1);
                if length(thisParam) > 2 then begin
                    param.Value := copy(thisParam, 3);
                end else begin
                    param.Value := '';
                end;
            end else begin
                WriteLn('Hyphen without switch contents found.');
                exit;
            end;
        end else begin
            if flgFoundName then begin
                writeln('Too many items without a switch!');
                exit();
            end;
            writeln('Found name of file to process: ', thisParam);
            flgFoundName := true;
            _Filename := thisParam;
        end;
        // TODO: Check for too many items without a switch
    end;
end;

end.
