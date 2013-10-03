unit sgDriverAudioSDLMixer;
//=============================================================================
// sgDriverAudioSDLMixer.pas
//=============================================================================
//
// This is responsible for providing the interface between the AudioDriver and
// the underlaying driver, SDL_Mixer 1.2
//
// It includes specific calls to SDL and SDL_Mixer, it also requires
// sgAudioDriver so that it can access the driver to Load itself.
// TODO:
//    - Write the rest of the procedures required in sgAudio
//    - Make the procedures/functions more generic by returning pointers
//=============================================================================
interface
  // Loads the AudioDriver with the procedures required to access SDL_Mixer
  procedure LoadSDLMixerAudioDriver();
  
implementation
  uses sgDriverAudio, sgTypes, sgShared, SDL, SDL_Mixer;
  
  var
        // Contains the sound channels used to determine if a sound is currently
        // playing and enables us to stop the sound, check if it is playing etc.
        soundChannels: Array[0..31] of Pointer;
    
//=============================================================================
// These procedures and functions provide an interface with SDL 1.2
//=============================================================================

  // GetChannel finds the audio channel that a sound effect is being played over
  // and returns the channel number. It returns -1 if the sound could not be found
  function GetChannel(effect : SoundEffect) : Integer;
  var
    i : Integer;
  begin
    result := -1;
    for i := Low(soundChannels) to High(soundChannels) do
    begin
      if (soundChannels[i] = effect) and (Mix_Playing(i) = 1) then
      begin
        result := i;
        break;
      end;
    end;
  end;

  // The OpenAudioProcedure is responsible for enabling audio to be used
  // it returns Boolean true if it succeeded and false if it failed.
  // SDL1.2 OpenAudio returns 0 for success and -1 for failure
  function OpenAudioProcedure() : Boolean;
  begin
    result := Mix_OpenAudio(MIX_DEFAULT_FREQUENCY, MIX_DEFAULT_FORMAT, 2, 2048 ) >= 0;
    if result then Mix_AllocateChannels(Length(soundChannels));
  end;
  
  procedure CloseAudioProcedure();
  begin
    Mix_CloseAudio();
  end;
  
  // GetErrorProcedure gets an audio error and returns the error as
  // a pascal string
  function GetErrorProcedure() : String;
  begin
    result := StrPas(MIX_GetError()); // Converts from PChar to Pascal String
  end;


//=============================================================================
//              Sound Effects
//=============================================================================

  // LoadSoundEffectProcedure is responsible for loading a SwinGame SoundEffect
  // and returning it, if it is unable to load the sound effect it raises an error
  // and returns a null pointer.
  function LoadSoundEffectProcedure(filename, name: String) : SoundEffect;
  begin
    New(result);        
    result^.effect := Mix_LoadWAV(PChar(filename));
    
    if result^.effect = nil then
    begin
      Dispose(result);
      result := nil;
      RaiseWarning('Error loading sound effect: ' + AudioDriver.GetError());
      exit;
    end;

    result^.filename := filename;
    result^.name := name;
  end;
  
  procedure StopSoundEffectProcedure(effect : SoundEffect);
  var
    channel : Integer;
  begin
    channel := GetChannel(effect);
    if channel >= 0 then
    begin
      Mix_HaltChannel(channel);
      soundChannels[channel] := nil;  // Sound effect is stopped and so remove it from the currently playing
    end;
  end;
  
  // FreeSoundEffectProcedure frees the memory of the sound effect
  procedure FreeSoundEffectProcedure(effect : SoundEffect);
  begin
    Mix_FreeChunk(effect^.effect);
  end;
  
  // PlaySoundEffectProcedure plays a sound effect a number of times
  // it returns the channel that the sound effect will be played on
  // returns Boolean false if there is an error and true if it suceeded
  function PlaySoundEffectProcedure(effect : SoundEffect; loops : Integer; volume : Single) : Boolean;
  var
    channel : Integer;
  begin
    result := false;
    channel := Mix_PlayChannel( -1, effect^.effect, loops);
    if (channel >= 0) then
    begin
      Mix_Volume(channel, RoundInt(volume * 128));
      soundChannels[channel] := effect;   // record which channel is playing the effect
      result := true;
    end;
  end;
  
  // Gets the volume of a sound effect, returns a negative single if 
  // the effect isn't playing.
  function GetSoundEffectVolumeProcedure(effect : SoundEffect) : Single;
  var
    channel : Integer;
  begin
    channel := GetChannel(effect);
    if channel >= 0 then
      result := Mix_Volume(channel, -1) / 128
    else
      result := -1.0
  end;
  
  procedure SetSoundEffectVolumeProcedure(effect : SoundEffect; newVolume : Single);
  var
    channel : Integer;
  begin
    channel := GetChannel(effect);
    if channel >= 0 then
      Mix_Volume(channel, RoundInt(newVolume * 128));
  end;
  
  // ChannelPlaying returns true if the channel is playing music and false if it is not
  function SoundEffectPlayingProcedure(effect : SoundEffect) : Boolean;
  var
    channel : Integer;
  begin
    result := false;
    channel := GetChannel(effect);
    if channel >= 0 then
    begin
      result := Mix_Playing(channel) = 1; // Playing returns 1 if the channel is playing
    end;
  end;
  

//=============================================================================
//              Music
//=============================================================================
  
  // MusicPlaying returns true if music is currently being played
  function MusicPlayingProcedure() : Boolean;
  begin
    result := Mix_PlayingMusic() = 1; // Playing music returns 1 if music is playing
  end;
  
  procedure PauseMusicProcedure();
  begin
    Mix_PauseMusic();
  end;
  
  procedure ResumeMusicProcedure();
  begin
    Mix_ResumeMusic();
  end;
  
  procedure StopMusicProcedure();
  begin
    Mix_HaltMusic();
  end;
  
  // LoadMusicEffectProocedure is responsible for loading a SwinGame Music
  // and returning it, if it is unable to load the sound effect it raises an error
  // and returns a null pointer.
  function LoadMusicProcedure(filename, name: String) : Music;
  begin
    New(result);        
    result^.music := Mix_LoadMUS(PChar(filename));
    
    if result^.music = nil then
    begin
      Dispose(result);
      result := nil;
      RaiseWarning('Error loading sound effect: ' + AudioDriver.GetError());
      exit;
    end;

    result^.filename := filename;
    result^.name := name;
  end;
  
  // FreeSoundEffectProcedure frees the memory of the sound effect
  // and sets the SoundEffect passed in to null
  procedure FreeMusicProcedure(music : Music);
  begin
    Mix_FreeMusic(music^.music);
    Dispose(music);
  end;
  
  procedure SetMusicVolumeProcedure(newVolume : Single);
  begin
    // if the current computer playing the music is not windows
    // the music can be played without checking type
    {$IFDEF UNIX}
    Mix_VolumeMusic(RoundInt(newVolume * 128));
    {$ELSE}
    // The music is being played on a Windows machine and so
    // if the music is a midi type then it shouldn't be allowed
    // to change volume.
    if Mix_GetMusicType(nil) <> MUS_MID then
    begin
      Mix_VolumeMusic(RoundInt(newVolume * 128));
    end;
    {$ENDIF}
  end;
  
  // GetMusicVolume returns the single that describes the current volume
  // of the music. Values between 0.0 and 1.0 are accepted.
  function GetMusicVolumeProcedure() : Single;
  begin
    result := Mix_VolumeMusic(-1)  / 128.0;
  end;  
    
  // PlayMusicProcedure plays music a number of times,
  // it returns a Boolean, true if it suceeded and false if it failed
  function PlayMusicProcedure(music : Music; loops : Integer) : Boolean;
  begin
    result := Mix_PlayMusic(music^.music, loops) >= 0; //PlayMusic returns 0 on success -1 on fail
  end;
  
  function FadeMusicInProcedure(music : Music; loops, ms : Integer) : Boolean;
  begin
    result := Mix_FadeInMusic(music^.music, loops, ms) >= 0; // FadeInMusic returns 0 on success -1 on fail
  end;
  
  function FadeMusicOutProcedure(ms : Integer) : Boolean;
  begin
    result := Mix_FadeOutMusic(ms) > 0; // FadeOutMusic returns 1 on success 0 on fail
  end;
  

//============================================================================= 
// Loads the SDL 1.2 procedures and functions into the audio driver
//=============================================================================
  procedure LoadSDLMixerAudioDriver();
  var
    i : Integer;
  begin
    for i := Low(soundChannels) to High(soundChannels) do
    begin
      soundChannels[i] := nil;
    end;
    //WriteLn('Loading SDL_Mixer Audio Driver...');
    AudioDriver.LoadSoundEffect := @LoadSoundEffectProcedure;   
    AudioDriver.OpenAudio := @OpenAudioProcedure;
    AudioDriver.CloseAudio := @CloseAudioProcedure;
    AudioDriver.SetMusicVolume := @SetMusicVolumeProcedure;
    AudioDriver.GetMusicVolume := @GetMusicVolumeProcedure;
    AudioDriver.GetError := @GetErrorProcedure;
    AudioDriver.FreeSoundEffect := @FreeSoundEffectProcedure;
    AudioDriver.LoadMusic := @LoadMusicProcedure;   
    AudioDriver.FreeMusic := @FreeMusicProcedure;
    AudioDriver.PlaySoundEffect := @PlaySoundEffectProcedure;
    AudioDriver.PlayMusic := @PlayMusicProcedure;
    AudioDriver.FadeMusicIn := @FadeMusicInProcedure;
    AudioDriver.FadeMusicOut := @FadeMusicOutProcedure;
    AudioDriver.SetSoundEffectVolume := @SetSoundEffectVolumeProcedure;
    AudioDriver.GetSoundEffectVolume := @GetSoundEffectVolumeProcedure;
    AudioDriver.SoundEffectPlaying := @SoundEffectPlayingProcedure;
    AudioDriver.MusicPlaying := @MusicPlayingProcedure;
    AudioDriver.PauseMusic := @PauseMusicProcedure;
    AudioDriver.ResumeMusic := @ResumeMusicProcedure;
    AudioDriver.StopMusic := @StopMusicProcedure;
    AudioDriver.StopSoundEffect := @StopSoundEffectProcedure;
  end;
end.