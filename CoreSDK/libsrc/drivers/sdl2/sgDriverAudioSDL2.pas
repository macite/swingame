unit sgDriverAudioSDL2;

interface
  // Loads the AudioDriver with the procedures required to access SDL_Mixer
  procedure LoadSDL2MixerAudioDriver();
  
implementation
  uses sgDriverAudio, sgTypes, sgShared, sysUtils, sgDriverSDL2Types, sgBackendTypes;
  
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

  function _LoadSoundData(const filename, name: String; kind: sg_sound_kind) : Pointer;
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
  function LoadSoundEffectProcedure(const filename, name: String) : SoundEffect;
  var
    s: SoundEffectPtr;
  begin
    //TODO: Move some of this to Audio unit
    New(s); 

    s^.effect := _LoadSoundData(filename, name, SGSD_SOUND_EFFECT);

    if s^.effect = nil then 
    begin
      s^.id := NONE_PTR;
      Dispose(s);
      s := nil;
    end
    else
    begin
      s^.id := AUDIO_PTR;
      s^.filename := filename;
      s^.name := name;
    end;

    result := s;
  end;
  
  function _Effect(sound: SoundEffect): Pointer;
  var
    s: SoundEffectPtr;
  begin
    s := ToSoundEffectPtr(sound);
    if Assigned(s) then result := s^.effect
    else result := nil;
  end;

  procedure StopSoundEffectProcedure(sound : SoundEffect);
  begin
    _sg_functions^.audio.stop_sound(_Effect(sound));
  end;
  
  procedure FreeSoundEffectProcedure(sound : SoundEffect);
  var
    sndData: ^sg_sound_data;
    s: SoundEffectPtr;
  begin
    s := ToSoundEffectPtr(sound);
    sndData := s^.effect;
    _sg_functions^.audio.close_sound_data(sndData);
    Dispose(sndData);
    s^.effect := nil;
    s^.id := NONE_PTR;
  end;
  
  function PlaySoundEffectProcedure(sound : SoundEffect; loops : Integer; volume : Single) : Boolean;
  begin
    _sg_functions^.audio.play_sound(_Effect(sound), loops, volume);
    result := true;
  end;
  
  // Gets the volume of a sound effect, returns a negative single if 
  // the effect isn't playing.
  function GetSoundEffectVolumeProcedure(sound : SoundEffect) : Single;
  begin
    result := _sg_functions^.audio.sound_volume(_Effect(sound));
  end;
  
  procedure SetSoundEffectVolumeProcedure(sound : SoundEffect; newVolume : Single);
  begin
    _sg_functions^.audio.set_sound_volume(_Effect(sound), newVolume);
  end;
  
  function SoundEffectPlayingProcedure(sound : SoundEffect) : Boolean;
  begin
    result := _sg_functions^.audio.sound_playing(_Effect(sound)) <> 0;
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
  function LoadMusicProcedure(const filename, name: String): Music;
  var
    m: MusicPtr;
  begin
    New(m); 

    m^.effect := _LoadSoundData(filename, name, SGSD_MUSIC);

    if m^.effect = nil then 
    begin
      m^.id := NONE_PTR;
      Dispose(m);
      m := nil;
    end
    else
    begin
      m^.id := MUSIC_PTR;
      m^.filename := filename;
      m^.name := name;
    end;

    result := m;
  end;

  function _Music(music: Music): Pointer;
  var
    m: MusicPtr;
  begin
    m := ToMusicPtr(music);
    if Assigned(m) then result := m^.effect
    else result := nil;
  end;
  
  procedure FreeMusicProcedure(music : Music);
  var
    m: MusicPtr;
    sndData: ^sg_sound_data;
  begin
    m := ToMusicPtr(music);

    sndData := m^.effect;
    _sg_functions^.audio.close_sound_data(sndData);
    Dispose(sndData);

    m^.effect := nil;
    m^.id := NONE_PTR;
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
    _sg_functions^.audio.play_sound(_Music(music), loops, pct);
    result := true;
  end;
  
  function FadeMusicInProcedure(music : Music; loops, ms : Integer) : Boolean;
  begin
    _sg_functions^.audio.fade_in(_Music(music), loops, ms);
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
