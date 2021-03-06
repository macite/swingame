Public Module GameMain
    Public Sub Main()
        'Open the game window
        OpenGraphicsWindow("GameMain", 800, 600)
        ShowSwinGameSplashScreen()
        
        'Run the game loop
        Do While Not WindowCloseRequested()
            'Fetch the next batch of UI interaction
            ProcessEvents()
            
            'Clear the screen and draw the framerate'
            ClearScreen(Color.White)
            DrawFramerate(0,0)
            
            'Draw onto the screen
            RefreshScreen(60)
        Loop
        
        'Close any resources we were using
        ReleaseAllResources()
    End Sub
End Module