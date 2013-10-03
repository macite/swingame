program SDL13AudioTest;
uses sgAudio;

	
procedure main();
begin
	
	Delay(200);
   
 // LoadSoundEffectNamed('boom', 'boom.wav');
//	LoadSoundEffectNamed('shock', 'shock.wav');
	
	CloseAudio();
	ReleaseAllResources();
end;

begin
	main();
end.