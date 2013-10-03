Public Module GameMain
    Public Sub Main()
        'Start the audio system.
        Audio.OpenAudio()
        
        'Open the game window
        Graphics.OpenGraphicsWindow("GameMain", 800, 600)
        Graphics.ShowSwinGameSplashScreen()
        
        'Run the game loop
        Do While Not Input.WindowCloseRequested()
            'Fetch the next batch of UI interaction
            Input.ProcessEvents()
            
            'Clear the screen and draw the framerate'
            Graphics.ClearScreen(Color.White)
            Text.DrawFramerate(0,0)
            
            'Draw onto the screen
            Graphics.RefreshScreen()
        Loop
        
        'End the audio
        Audio.CloseAudio()
        
        'Close any resources we were using
        Resources.ReleaseAllResources()
    End Sub
End Module