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

	function LoadSoundEffectProcedure(filename, name: String) : SoundEffect;
	var
		sndData: ^sg_sound_data;
	begin
		//TODO: Move some of this to Audio unit

		New(sndData);
		New(result); 

		sndData^ := _sg_functions^.audio.load_sound_data(PChar(filename), SGSD_SOUND_EFFECT);
		result^.effect := sndData;

		if sndData^._data = nil then 
		begin
			Dispose(result);
			Dispose(sndData);
			result := nil;
			RaiseWarning('Error loading sound effect: ' + name + ' (' + filename + ')');
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
		result := false;
	end;
	
	procedure PauseMusicProcedure();
	begin
	end;
	
	procedure ResumeMusicProcedure();
	begin
	end;
	
	procedure StopMusicProcedure();
	begin
	end;
	
	function LoadMusicProcedure(filename, name: String) : Music;
	begin
		result := nil;
	end;
	
	procedure FreeMusicProcedure(music : Music);
	begin
	end;
	
	procedure SetMusicVolumeProcedure(newVolume : Single);
	begin
	end;
	
	function GetMusicVolumeProcedure() : Single;
	begin
		result := 0;
	end;  
		
	function PlayMusicProcedure(music : Music; loops : Integer) : Boolean;
	begin
		result := false;
	end;
	
	function FadeMusicInProcedure(music : Music; loops, ms : Integer) : Boolean;
	begin
		result := false;
	end;
	
	function FadeMusicOutProcedure(ms : Integer) : Boolean;
	begin
		result := false;
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
