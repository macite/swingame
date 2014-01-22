unit sgDriverAudioSDL2;

interface
	// Loads the AudioDriver with the procedures required to access SDL_Mixer
	procedure LoadSDL2MixerAudioDriver();
	
implementation
	uses sgDriverAudio, sgTypes, sgShared, sysUtils, sgDriverSDL2Types;
	
	function OpenAudioProcedure() : Boolean;
	begin
		_sg_functions^.audio.open_audio();
		result := true;
	end;
	
	procedure CloseAudioProcedure();
	begin
		_sg_functions^.audio.close_audio();
	end;
	
	// GetErrorProcedure gets an audio error and returns the error as
	// a pascal string
	function GetErrorProcedure() : String;
	begin
		result := 'TODO: ERROR MESSAGE'; //TODO: add error message
	end;

//=============================================================================
//              Sound Effects
//=============================================================================

	function _LoadSoundData(filename, name: String; kind: sg_sound_kind) : Pointer;
	var
		sndData: ^sg_sound_data;
	begin
		New(sndData);
		result := sndData;

		sndData^ := _sg_functions^.audio.load_sound_data(PChar(filename), kind);

		if sndData^._data = nil then 
		begin
			Dispose(sndData);
			result := nil;
			RaiseWarning('Error loading sound data for ' + name + ' (' + filename + ')');
		end;
	end;

	//TODO: most of this can be moved to sgAudio
	function LoadSoundEffectProcedure(filename, name: String) : SoundEffect;
	begin
		//TODO: Move some of this to Audio unit
		New(result); 

		result^.effect := _LoadSoundData(filename, name, SGSD_SOUND_EFFECT);

		if result^.effect = nil then 
		begin
			Dispose(result);
			result := nil;
		end
		else
		begin
			result^.filename := filename;
			result^.name := name;
		end;
	end;
	
	procedure StopSoundEffectProcedure(sound : SoundEffect);
	begin
		_sg_functions^.audio.stop_sound(sound^.effect);
	end;
	
	procedure FreeSoundEffectProcedure(sound : SoundEffect);
	var
		sndData: ^sg_sound_data;
	begin
		sndData := sound^.effect;
		_sg_functions^.audio.close_sound_data(sndData);
		Dispose(sndData);
		sound^.effect := nil;
	end;
	
	function PlaySoundEffectProcedure(sound : SoundEffect; loops : Integer; volume : Single) : Boolean;
	begin
		_sg_functions^.audio.play_sound(sound^.effect, loops, volume);
		result := true;
	end;
	
	// Gets the volume of a sound effect, returns a negative single if 
	// the effect isn't playing.
	function GetSoundEffectVolumeProcedure(sound : SoundEffect) : Single;
	begin
		result := _sg_functions^.audio.sound_volume(sound^.effect);
	end;
	
	procedure SetSoundEffectVolumeProcedure(sound : SoundEffect; newVolume : Single);
	begin
		_sg_functions^.audio.set_sound_volume(sound^.effect, newVolume);
	end;
	
	function SoundEffectPlayingProcedure(sound : SoundEffect) : Boolean;
	begin
		result := _sg_functions^.audio.sound_playing(sound^.effect) <> 0;
	end;
	
	//=============================================================================
	//              Music
	//=============================================================================
	
	// MusicPlaying returns true if music is currently being played
	function MusicPlayingProcedure() : Boolean;
	begin
		result := _sg_functions^.audio.music_playing() <> 0;
	end;
	
	procedure PauseMusicProcedure();
	begin
		_sg_functions^.audio.pause_music();
	end;
	
	procedure ResumeMusicProcedure();
	begin
		_sg_functions^.audio.resume_music();
	end;
	
	procedure StopMusicProcedure();
	begin
		_sg_functions^.audio.stop_music();
	end;
	
	//TODO: Move this to Audio unit
	function LoadMusicProcedure(filename, name: String) : Music;
	begin
		New(result); 

		result^.music := _LoadSoundData(filename, name, SGSD_MUSIC);

		if result^.music = nil then 
		begin
			Dispose(result);
			result := nil;
		end
		else
		begin
			result^.filename := filename;
			result^.name := name;
		end;
	end;
	
	procedure FreeMusicProcedure(music : Music);
	var
		sndData: ^sg_sound_data;
	begin
		sndData := music^.music;
		_sg_functions^.audio.close_sound_data(sndData);
		Dispose(sndData);
		music^.music := nil;
	end;
	
	procedure SetMusicVolumeProcedure(newVolume : Single);
	begin
		_sg_functions^.audio.set_music_vol(newVolume);
	end;
	
	function GetMusicVolumeProcedure() : Single;
	begin
		result := _sg_functions^.audio.music_vol();
	end;  
		
	function PlayMusicProcedure(music : Music; loops : Integer) : Boolean;
	var
		pct: Single;
	begin
		pct := GetMusicVolumeProcedure();
		_sg_functions^.audio.play_sound(music^.music, loops, pct);
		result := true;
	end;
	
	function FadeMusicInProcedure(music : Music; loops, ms : Integer) : Boolean;
	begin
		_sg_functions^.audio.fade_in(music^.music, loops, ms);
		result := true;
	end;
	
	function FadeMusicOutProcedure(ms : Integer) : Boolean;
	begin
		WriteLn('fade out');
		_sg_functions^.audio.fade_out(_sg_functions^.audio.current_music(), ms);
		result := true;
	end;
	
	//============================================================================= 
	// Loads the procedures and functions into the audio driver
	//=============================================================================
	procedure LoadSDL2MixerAudioDriver();
	begin
		//WriteLn('Loading SDL_Mixer Audio Driver...');
		AudioDriver.LoadSoundEffect := @LoadSoundEffectProcedure;   // # 
		AudioDriver.OpenAudio := @OpenAudioProcedure;               // #
		AudioDriver.CloseAudio := @CloseAudioProcedure;             // #
		AudioDriver.SetMusicVolume := @SetMusicVolumeProcedure;     // #    
		AudioDriver.GetMusicVolume := @GetMusicVolumeProcedure;     // #
		AudioDriver.GetError := @GetErrorProcedure;
		AudioDriver.FreeSoundEffect := @FreeSoundEffectProcedure;   // #
		AudioDriver.LoadMusic := @LoadMusicProcedure;               // #   
		AudioDriver.FreeMusic := @FreeMusicProcedure;               // #
		AudioDriver.PlaySoundEffect := @PlaySoundEffectProcedure;   // #
		AudioDriver.PlayMusic := @PlayMusicProcedure;               // #
		AudioDriver.FadeMusicIn := @FadeMusicInProcedure;           // #
		AudioDriver.FadeMusicOut := @FadeMusicOutProcedure;         // # + all snd effects
		//TODO: Use this!
		AudioDriver.SetSoundEffectVolume := @SetSoundEffectVolumeProcedure; // #
		AudioDriver.GetSoundEffectVolume := @GetSoundEffectVolumeProcedure; // #
		AudioDriver.SoundEffectPlaying := @SoundEffectPlayingProcedure;     // #
		AudioDriver.MusicPlaying := @MusicPlayingProcedure;         // #
		AudioDriver.PauseMusic := @PauseMusicProcedure;             // #
		AudioDriver.ResumeMusic := @ResumeMusicProcedure;           // #
		AudioDriver.StopMusic := @StopMusicProcedure;               // #
		AudioDriver.StopSoundEffect := @StopSoundEffectProcedure;   // #
	end;
end.
