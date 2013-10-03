unit sgDriverAudio;
//=============================================================================
// sgDriverAudio.pas
//=============================================================================
//
// The Audio Driver is responsible for providing an interface between SwinGame
// code and the drivers. It can interface between any audio driver provided
// It should not be changed, any changes to this file will probably cause all
// the audio drivers to break.
// 
// 
//
// Notes:
//		- Pascal PChar is equivalent to a C-type string
// 		- Pascal Word is equivalent to a Uint16
//		- Pascal LongWord is equivalent to a Uint32
//
//=============================================================================

interface
	uses sgTypes, {$IFDEF SWINGAME_SDL13}sgDriverAudioSDL13Mixer{$ELSE}sgDriverAudioSDLMixer{$ENDIF};
	
	type
		// These function and procedure pointers are required by the AudioDriverRecord
		OpenAudioProcedure = function () : Boolean;
		CloseAudioProcedure = procedure ();
		LoadSoundEffectProcedure = function (filename, name : String) : SoundEffect;
		SetMusicVolumeProcedure = procedure (newVolume : Single);
		GetMusicVolumeProcedure = function () : Single;
		GetErrorProcedure = function () : String;
		FreeSoundEffectProcedure = procedure (effect : SoundEffect);
		LoadMusicProcedure = function (filename, name : String) : Music;
		FreeMusicProcedure = procedure (music : Music);
		PlaySoundEffectProcedure = function (effect : SoundEffect; loops : Integer; volume : Single) : Boolean;
		PlayMusicProcedure = function (music : Music; loops : Integer) : Boolean;
		FadeMusicInProcedure = function (music : Music; loops, ms : Integer) : Boolean;
		FadeMusicOutProcedure = function (ms : Integer) : Boolean;
		GetSoundEffectVolumeProcedure = function(effect : SoundEffect) : Single;
		SetSoundEffectVolumeProcedure = procedure(effect : SoundEffect; newVolume : Single);
		SoundEffectPlayingProcedure = function(effect : SoundEffect) : Boolean;
		MusicPlayingProcedure = function() : Boolean;
		PauseMusicProcedure = procedure();
		ResumeMusicProcedure = procedure();
		StopMusicProcedure = procedure();
		StopSoundEffectProcedure = procedure(effect : SoundEffect);
		
		// Stores the functions and procedures from the current audio driver
		// So that calling AudioDriver.OpenAudio() will call the appropriate method
		// from the driver.
		// The names of the record fields should be clear and indicitive of what the
		// procedure does or should do.
		AudioDriverRecord = record
			OpenAudio : OpenAudioProcedure;
			CloseAudio : CloseAudioProcedure;
			LoadSoundEffect : LoadSoundEffectProcedure;
			SetMusicVolume : SetMusicVolumeProcedure;
			GetMusicVolume : GetMusicVolumeProcedure;
			GetError : GetErrorProcedure;
			FreeSoundEffect : FreeSoundEffectProcedure;
			LoadMusic : LoadMusicProcedure;
			FreeMusic : FreeMusicProcedure;
			PlaySoundEffect : PlaySoundEffectProcedure;
			PlayMusic : PlayMusicProcedure;
			FadeMusicIn : FadeMusicInProcedure;
			FadeMusicOut : FadeMusicOutProcedure;
			GetSoundEffectVolume : GetSoundEffectVolumeProcedure;
			SetSoundEffectVolume : SetSoundEffectVolumeProcedure;
			SoundEffectPlaying : SoundEffectPlayingProcedure;
			MusicPlaying : MusicPlayingProcedure;
			PauseMusic : PauseMusicProcedure;
			ResumeMusic : ResumeMusicProcedure;
			StopMusic : StopMusicProcedure;
			StopSoundEffect : StopSoundEffectProcedure;
		end;
		
	var
		// Global variable used to allow SwinGame to access the functions and procedures
		// of the audio driver.
		AudioDriver : AudioDriverRecord;
		
implementation
//=============================================================================
// Default functions and procedures that allow lazy loading of the AudioDriver
// They are loaded into the AudioiDriver by default and when called load the 
// current default audio driver. They then call the audio driver again to make
// sure the appropriate action is completed.
// Naming : Default____Procedure
//=============================================================================
// TODO:
// 		- Create LoadDriver function that accepts an enum describing the driver
//			to load. This function will replace LoadDefaultAudioDriver()
//=============================================================================
	procedure LoadDefaultAudioDriver();
	begin
	  {$IFDEF SWINGAME_SDL13}
	    LoadSDL13MixerAudioDriver();
	  {$ELSE}
	    LoadSDLMixerAudioDriver();
	  {$ENDIF};
	end;

//=============================================================================
	function DefaultOpenAudioProcedure() : Boolean;
	begin
		LoadDefaultAudioDriver();
		result := AudioDriver.OpenAudio();
	end;
	
	procedure DefaultCloseAudioProcedure();
	begin
		LoadDefaultAudioDriver();
		AudioDriver.CloseAudio();
	end;
	
	function DefaultLoadSoundEffectProcedure(filename, name : String) : SoundEffect;
	begin
		LoadDefaultAudioDriver();
		result := AudioDriver.LoadSoundEffect(filename, name);
	end;
	
	procedure DefaultSetMusicVolumeProcedure(newVolume : Single);
	begin
		LoadDefaultAudioDriver();
		AudioDriver.SetMusicVolume(newVolume);
	end;
	
	function DefaultGetMusicVolumeProcedure() : Single;
	begin
		LoadDefaultAudioDriver();
		result := AudioDriver.GetMusicVolume();
	end;
	
	function DefaultGetErrorProcedure() : String;
	begin
		LoadDefaultAudioDriver();
		result := AudioDriver.GetError();
	end;
	
	procedure DefaultFreeSoundEffectProcedure(effect : SoundEffect);
	begin
		LoadDefaultAudioDriver();
		AudioDriver.FreeSoundEffect(effect);
	end;
	
	function DefaultLoadMusicProcedure(filename, name : String) : Music;
	begin
		LoadDefaultAudioDriver();
		result := AudioDriver.LoadMusic(filename, name);
	end;
	
	procedure DefaultFreeMusicProcedure(music : Music);
	begin
		LoadDefaultAudioDriver();
		AudioDriver.FreeMusic(music);
	end;
	
	function DefaultPlaySoundEffectProcedure(effect : SoundEffect; loops : Integer; volume : Single) : Boolean;
	begin
		LoadDefaultAudioDriver();
		result := AudioDriver.PlaySoundEffect(effect, loops, volume);
	end;
	
	function DefaultPlayMusicProcedure(music : Music; loops : Integer) : Boolean;
	begin
		LoadDefaultAudioDriver();
		result := AudioDriver.PlayMusic(music, loops);
	end;
	
	function DefaultFadeMusicInProcedure(music : Music; loops, ms : Integer) : Boolean;
	begin
		LoadDefaultAudioDriver();
		result := AudioDriver.FadeMusicIn(music, loops, ms);
	end;
	
	function DefaultFadeMusicOutProcedure(ms : Integer) : Boolean;
	begin
		LoadDefaultAudioDriver();
		result := AudioDriver.FadeMusicOut(ms);
	end;
	
	procedure DefaultSetSoundEffectVolumeProcedure(effect : SoundEffect; newVolume : Single);
	begin
		LoadDefaultAudioDriver();
		AudioDriver.SetSoundEffectVolume(effect, newVolume);
	end;
	
	function DefaultGetSoundEffectVolumeProcedure(effect : SoundEffect) : Single;
	begin
		LoadDefaultAudioDriver();
		result := AudioDriver.GetSoundEffectVolume(effect);
	end;
	
	function DefaultSoundEffectPlayingProcedure(effect : SoundEffect) : Boolean;
	begin
		LoadDefaultAudioDriver();
		result := AudioDriver.SoundEffectPlaying(effect);
	end;
	
	function DefaultMusicPlayingProcedure() : Boolean;
	begin
		LoadDefaultAudioDriver();
		result := AudioDriver.MusicPlaying();	
	end;
	
	procedure DefaultPauseMusicProcedure();
	begin
		LoadDefaultAudioDriver();
		AudioDriver.PauseMusic();
	end;
	
	procedure DefaultResumeMusicProcedure();
	begin
		LoadDefaultAudioDriver();
		AudioDriver.ResumeMusic();
	end;
	
	procedure DefaultStopMusicProcedure();
	begin
		LoadDefaultAudioDriver();
		AudioDriver.StopMusic();
	end;
	
	procedure DefaultStopSoundEffectProcedure(effect : SoundEffect);
	begin
		LoadDefaultAudioDriver();
		AudioDriver.StopSoundEffect(effect);
	end;
//=============================================================================
	
	initialization
	begin
		AudioDriver.OpenAudio := @DefaultOpenAudioProcedure;
		AudioDriver.CloseAudio := @DefaultCloseAudioProcedure;
		AudioDriver.LoadSoundEffect := @DefaultLoadSoundEffectProcedure;
		AudioDriver.SetMusicVolume := @DefaultSetMusicVolumeProcedure;
		AudioDriver.GetMusicVolume := @DefaultGetMusicVolumeProcedure;
		AudioDriver.GetError := @DefaultGetErrorProcedure;
		AudioDriver.FreeSoundEffect := @DefaultFreeSoundEffectProcedure;
		AudioDriver.LoadMusic := @DefaultLoadMusicProcedure;
		AudioDriver.FreeMusic := @DefaultFreeMusicProcedure;
		AudioDriver.PlaySoundEffect := @DefaultPlaySoundEffectProcedure;
		AudioDriver.PlayMusic := @DefaultPlayMusicProcedure;
		AudioDriver.FadeMusicIn := @DefaultFadeMusicInProcedure;
		AudioDriver.FadeMusicOut := @DefaultFadeMusicOutProcedure;
		AudioDriver.SetSoundEffectVolume := @DefaultSetSoundEffectVolumeProcedure;
		AudioDriver.GetSoundEffectVolume := @DefaultGetSoundEffectVolumeProcedure;
		AudioDriver.SoundEffectPlaying := @DefaultSoundEffectPlayingProcedure;
		AudioDriver.MusicPlaying := @DefaultMusicPlayingProcedure;
		AudioDriver.PauseMusic := @DefaultPauseMusicProcedure;
		AudioDriver.ResumeMusic := @DefaultResumeMusicProcedure;
		AudioDriver.StopMusic := @DefaultStopMusicProcedure;
		AudioDriver.StopSoundEffect := @DefaultStopSoundEffectProcedure;
	end;
end.