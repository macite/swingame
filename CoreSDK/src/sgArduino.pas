//=============================================================================
// sgArduino.pas
//=============================================================================

/// SwinGame's Arduino unit is capable of connecting and communicating
/// with an Arduino device using a Serial port.
///
/// @module Arduino
/// @static
///
/// @doc_types ArduinoDevice
unit sgArduino;

interface
	uses sgTypes;

	/// Creates an Arduino device at the specified port, with
	/// the indicated baud. The name of the device matches its port.
	///
	/// @lib
	/// @sn createArduinoOnPort:%s atBaud:%s
	///
	/// @class ArduinoDevice
	/// @constructor
	/// @csn initOnPort:%s atBaud:%s
	function CreateArduinoDevice(const port: String; baud: LongInt) : ArduinoDevice;

	/// Creates an Arduino device with the given name, 
	/// at the specified port, with the indicated baud.
	///
	/// @lib CreateArduinoNamed
	/// @sn createArduinoNamed:%s onPort:%s atBaud:%s 
	///
	/// @class ArduinoDevice
	/// @constructor
	/// @csn initWithName:%s OnPort:%s atBaud:%s
	function CreateArduinoDevice(const name, port: String; baud: LongInt) : ArduinoDevice;

	/// Returns the ArduinoDevice with the indicated name.
	///
	/// @lib
	function ArduinoDeviceNamed(const name: String): ArduinoDevice;

	/// Does an ArduinoDevice exist with the indicated name?
	///
	/// @lib
	function HasArduinoDevice(const name: String): Boolean;

	/// Release the ArduinoDevice with the indicated name.
	///
	/// @lib
	procedure ReleaseArduinoDevice(const name: String);

	/// Close the connection to the Arduino Device and dispose
	/// of the resources associated with the Device.
	/// 
	/// @lib
	/// @sn ArduinoCloseConnection:%s
	///
	/// @class ArduinoDevice
	/// @dispose
	procedure FreeArduinoDevice(var dev: ArduinoDevice);

	/// Reads a line of text from the ArduinoDevice. This
	/// returns an empty string if nothing is read within a
	/// few milliseconds.
	/// 
	/// @lib
	///
	/// @class ArduinoDevice
	/// @method ReadLine
	function ArduinoReadLine(dev: ArduinoDevice): String;

	/// Reads a line of text from the ArduinoDevice, within a 
	/// specified amount of time.
	///
	/// @lib ArduinoReadLineTimeout
	/// @sn arduinoReadLine:%s timeout:%s
	///
	/// @class ArduinoDevice
	/// @overload ReadLine ReadLineTimeout
	/// @csn readLineWithTimeout:%s
	function ArduinoReadLine(dev: ArduinoDevice; timeout: LongInt): String;

	/// Read a Byte from the ArduinoDevice. Has a short
	/// timeout and returns 0 if no byte is read within the given time.
	/// 
	/// @lib
	///
	/// @class ArduinoDevice
	/// @method ReadByte
	function ArduinoReadByte(dev: ArduinoDevice): Byte; overload;

	/// Reads a byte from the ArduinoDevice, with the given timeout in milliseconds.
	/// Returns 0 if no byte is read within the given time.
	/// 
	/// @lib ArduinoReadByteTimeout
	/// @sn arduinoReadByte:%s timeout:%s
	///
	/// @class ArduinoDevice
	/// @overload ReadByte ReadByteTimeout
	/// @csn readByteWithTimeout:%s
	function ArduinoReadByte(dev: ArduinoDevice; timeout: LongInt): Byte; overload;


	/// Send a byte value to the arduino device.
	///
	/// @lib
	/// @sn arduinoSendByte:%s value:%s
	///
	/// @class ArduinoDevice
	/// @method SendByte
	procedure ArduinoSendByte(dev: ArduinoDevice; value: Byte); overload;

	/// Send a string value to the arduino device.
	///
	/// @lib
	/// @sn arduinoSendString:%s value:%s
	///
	/// @class ArduinoDevice
	/// @method SendString
	procedure ArduinoSendString(dev: ArduinoDevice; const value: String);

	/// Send a string value to the arduino device, along with a newline
	/// so the arduino can identify the end of the sent data.
	///
	/// @lib
	/// @sn arduinoSendStringLine:%s value:%s
	///
	/// @class ArduinoDevice
	/// @method SendStringLine
	procedure ArduinoSendStringLine(dev: ArduinoDevice; const value: String);

	/// Returns true if there is data waiting to be read from the device.
	///
	/// @lib
	///
	/// @class ArduinoDevice
	/// @getter HasData
	function ArduinoHasData(dev: ArduinoDevice): Boolean;

	/// Release all of the ArduinoDevices
	/// 
	/// @lib
	procedure ReleaseAllArduinoDevices();

implementation
uses
	SysUtils, Classes, Synaser, sgShared, sgUtils, stringhash, sgBackendTypes;

    var _ArduinoDevices: TStringHash;


	function CreateArduinoDevice(const port: String; baud: LongInt) : ArduinoDevice;
	begin
		result := CreateArduinoDevice(port, port, baud);
	end;

	function CreateArduinoDevice(const name, port: String; baud: LongInt) : ArduinoDevice;
    var
        obj: tResourceContainer;
	    ser: TBlockSerial;
	    ap: ArduinoPtr;
	begin
        if HasArduinoDevice(name) then
        begin
            result := ArduinoDeviceNamed(name);
            exit;
        end;

		New(ap);
		result := ap;

		ap^.id := ARDUINO_PTR;
		ap^.name := name;
		ap^.ptr := Pointer(TBlockSerial.Create());
		ap^.port := port;
		ap^.baud := baud;
		ap^.open := false;
		ap^.hasError := false;
		ap^.errorMessage := 'Working';

		obj := tResourceContainer.Create(ap);
        if not _ArduinoDevices.setValue(name, obj) then
        begin
            RaiseWarning('** Leaking: Caused by ArduinoDevice resource loading twice, ' + name);
            result := nil;
            exit;
        end;

		if assigned(ap) then
		begin
			ser := TBlockSerial(ap^.ptr);
		    // WriteLn('Connecting...');
		    ser.Connect(port);

		    // WriteLn('Configure...');
		    ser.Config(baud, 8, 'N', SB1, False, False);
		    if ser.LastError <> sOK then
		    begin
    		    RaiseWarning('Error configuring connection to Arduino: ' + IntToStr(ser.LastError) + ' ' + ser.LastErrorDesc);
		    	ap^.hasError := true;
		    	ap^.errorMessage := ser.LastErrorDesc;
		    	exit;
		    end;
	    end;
	end;

	function _Serial(dev: ArduinoDevice): TBlockSerial;
	var
		ap: ArduinoPtr;
	begin
		ap := ToArduinoPtr(dev);
		if Assigned(ap) then result := TBlockSerial(ap^.ptr)
		else result := nil;
	end;

	function ArduinoHasData(dev: ArduinoDevice): Boolean;
	var
	    ser: TBlockSerial;	
	begin
		result := false;
		if assigned(dev) then
		begin
			ser := _Serial(dev);
			// result := ser.CanRead(0);
			result := ser.WaitingData() > 0;
		end;
	end;

	function ArduinoReadLine(dev: ArduinoDevice): String;
	begin
		result := ArduinoReadLine(dev, 10);
	end;

	function ArduinoReadLine(dev: ArduinoDevice; timeout: LongInt): String;
	var
	    ser: TBlockSerial;	
	begin
		result := '';
		if assigned(dev) then
		begin
			ser := _Serial(dev);
			result := ser.RecvString(timeout)
		end;
	end;

	function ArduinoReadByte(dev: ArduinoDevice): Byte; overload;
	begin
		result := ArduinoReadByte(dev, 10);
	end;

	function ArduinoReadByte(dev: ArduinoDevice; timeout: LongInt): Byte; overload;
	var
	    ser: TBlockSerial;	
	begin
		result := 0;
		if assigned(dev) then
		begin
			ser := _Serial(dev);
			result := ser.RecvByte(timeout);
		end;
	end;

    procedure ArduinoSendByte(dev: ArduinoDevice; value: Byte); overload;
	var
	    ser: TBlockSerial;	
	begin
		if assigned(dev) then
		begin
			ser := _Serial(dev);
			ser.SendByte(value);
		end;
	end;

    procedure ArduinoSendStringLine(dev: ArduinoDevice; const value: String);
	var
	    ser: TBlockSerial;	
	begin
		if assigned(dev) then
		begin
			ser := _Serial(dev);
			ser.SendString(value);
			ser.SendString(#13#10);
		end;
	end;

    procedure ArduinoSendString(dev: ArduinoDevice; const value: String);
	var
	    ser: TBlockSerial;	
	begin
		if assigned(dev) then
		begin
			ser := _Serial(dev);
			ser.SendString(value);
		end;
	end;


	// ========================
	// = Resource Management Routines
	// ========================	


    // private:
    // Called to actually free the resource
    procedure DoFreeArduinoDevice(var dev: ArduinoPtr);
	var
	    ser: TBlockSerial;	
    begin
        if assigned(dev) then
        begin
            CallFreeNotifier(dev);
			ser := _Serial(dev);
			ser.Free();
			dev^.ptr := nil;
            Dispose(dev);
        end;

        dev := nil;
    end;
    
    procedure FreeArduinoDevice(var dev: ArduinoDevice);
    var
    	ap: ArduinoPtr;
	begin
		ap := ToArduinoPtr(dev);

		if assigned(ap) then
		begin
			ReleaseArduinoDevice(ap^.name);
		end;

        dev := nil;
    end;
    
    procedure ReleaseArduinoDevice(const name: String);
    var
        dev: ArduinoDevice;
        ap: ArduinoPtr;
    begin
        dev := ArduinoDeviceNamed(name);
        if assigned(dev) then
        begin
            _ArduinoDevices.remove(name).Free();
            DoFreeArduinoDevice(ap);
        end;
    end;
    
    procedure ReleaseAllArduinoDevices();
    begin
        ReleaseAll(_ArduinoDevices, @ReleaseArduinoDevice);
    end;

    function HasArduinoDevice(const name: String): Boolean;
    begin
        result := _ArduinoDevices.containsKey(name);
    end;

    function ArduinoDeviceNamed(const name: String): ArduinoDevice;
    var
        tmp : TObject;
    begin
        tmp := _ArduinoDevices.values[name];
        if assigned(tmp) then 
        	result := ArduinoDevice(tResourceContainer(tmp).Resource)
        else result := nil;
    end;

initialization
    begin
        InitialiseSwinGame();
        _ArduinoDevices := TStringHash.Create(False, 1024);
    end;
    
    finalization
    begin
        ReleaseAllArduinoDevices();
        FreeAndNil(_ArduinoDevices);
    end;
end.

