//
// SpritePacks allow sprites to be grouped together and to have 
// procedures called on each element without the programmer needing
// to maintain an array.
//
unit SpritePack;
interface
	uses sgTypes, stringhash;

	type
		// The container for the SpritePacks, allowing it to be
		// stored in a HashTable
		//
	    TSpritePack = class(TObject)
	    private
	      _sprites: array of Sprite;
	      _name: String;

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
		SetLength(_sprites, 0);
	end;

	destructor TSpritePack.Destroy();
	begin
		// At this point sprites are not owned by their SpritePack... maybe they should be
		SetLength(_sprites, 0);
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
	begin
		SetLength(_sprites, Length(_sprites) + 1);
		_sprites[High(_sprites)] := s;
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

		for i := removeIdx to High(_sprites) - 1 do
		begin
			_sprites[i] := _sprites[i + 1];
		end;
		SetLength(_sprites, Length(_sprites) - 1);
	end;

	procedure TSpritePack.CallForAllSprites(fn: SpriteFunction);
	var
		i: Integer;
	begin
		for i := 0 to High(_sprites) do
		begin
			fn(_sprites[i]);
		end;
	end;

	procedure TSpritePack.CallForAllSprites(fn: SpriteSingleFunction; val: Single);
	var
		i: Integer;
	begin
		for i := 0 to High(_sprites) do
		begin
			fn(_sprites[i], val);
		end;
	end;
	
end.
		