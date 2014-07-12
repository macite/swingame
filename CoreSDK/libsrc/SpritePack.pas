//
// SpritePacks allow sprites to be grouped together and to have 
// procedures called on each element without the programmer needing
// to maintain an array.
//
unit SpritePack;
interface
	uses sgTypes, stringhash;

	type
		SpriteArray = array of Sprite;

		// The container for the SpritePacks, allowing it to be
		// stored in a HashTable
		//
	    TSpritePack = class(TObject)
	    private
	      _sprites: SpriteArray;

	      // Copy of _sprites for looping code to use
	      // this allows _sprites to change due to 
	      // user actions on the callback without
	      // it affecting the loop
	      _spritesBk: SpriteArray;
	      _name: String;
	      _inLoop, _spritesChanged: Boolean;

	      procedure EndLoop();

	    public
	      constructor Create(name: String);
	      destructor Destroy; override;

	      property Name: String read _name;

	      procedure AddPackTo(ht: TStringHash);

	      procedure AddSprite(s: Sprite);
	      procedure RemoveSprite(s: Sprite);

	      procedure CallForAllSprites(fn: SpriteFunction);
	      procedure CallForAllSprites(fn: SpriteSingleFunction; val: Single);
	    end;
	
implementation
	uses sgSprites, sgShared;
	
	constructor TSpritePack.Create(name: String);
	begin
		inherited Create();

		_name := name;
		_inLoop := false;
		_spritesChanged := false;

		SetLength(_sprites, 0);
		SetLength(_spritesBk, 0);
	end;

	destructor TSpritePack.Destroy();
	begin
		// At this point sprites are not owned by their SpritePack... maybe they should be
		SetLength(_sprites, 0);
		SetLength(_spritesBk, 0);
		inherited Destroy();
	end;

	procedure TSpritePack.AddPackTo(ht: TStringHash);
	begin
		if ht.containsKey(_name) then
		begin
			RaiseWarning('Attempting to create duplicate SpritePack named ' + _name);
			exit;
		end;

		ht.SetValue(_name, self);
	end;

	procedure TSpritePack.AddSprite(s: Sprite);
	 procedure AddOp(var arr: SpriteArray);
	 begin
	 	SetLength(arr, Length(arr) + 1);
		arr[High(arr)] := s;
	 end;
	begin
		AddOp(_sprites);
		AddOp(_spritesBk); // allow add in loops
	end;

	procedure TSpritePack.EndLoop();
	var
		i: Integer;
	begin
		_inLoop := false;
		if _spritesChanged then
		begin
			SetLength(_spritesBk, Length(_sprites));
			for i := 0 to High(_spritesBk) do
			begin
				_spritesBk[i] := _sprites[i];
			end;
			_spritesChanged := false;
		end;
	end;

	procedure TSpritePack.RemoveSprite(s: Sprite);
	var
		removeIdx, i: Integer;
	begin
		removeIdx := -1;
		for i := 0 to High(_sprites) do
		begin
			if _sprites[i] = s then
			begin
				removeIdx := i;
				break;
			end;
		end;

		if removeIdx < 0 then
		begin
			RaiseWarning('Attempted to remove sprite from incorrect pack!');
			exit;
		end;

		if _inLoop then _spritesChanged := true;

		for i := removeIdx to High(_sprites) - 1 do
		begin
			_sprites[i] := _sprites[i + 1];
			if not _inLoop then _spritesBk[i] := _spritesBk[i + 1];
		end;

		SetLength(_sprites, Length(_sprites) - 1);
		if not _inLoop then SetLength(_spritesBk, Length(_spritesBk) - 1);
	end;

	procedure TSpritePack.CallForAllSprites(fn: SpriteFunction);
	var
		i: Integer;
	begin
		_inLoop := true;

		for i := 0 to High(_spritesBk) do
		begin
			fn(_spritesBk[i]);
		end;
		EndLoop();
	end;

	procedure TSpritePack.CallForAllSprites(fn: SpriteSingleFunction; val: Single);
	var
		i: Integer;
	begin
		_inLoop := true;
		for i := 0 to High(_spritesBk) do
		begin
			fn(_spritesBk[i], val);
		end;
		EndLoop();
	end;
	
end.
		