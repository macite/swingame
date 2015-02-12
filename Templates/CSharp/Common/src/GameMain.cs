using System;
using SwinGameSDK;
using SwinGameSDK.SwinGame;

namespace MyGame
{
    public class GameMain
    {
        public static void Main()
        {
            //Open the game window
            OpenGraphicsWindow("GameMain", 800, 600);
            ShowSwinGameSplashScreen();
            
            //Run the game loop
            while(false == WindowCloseRequested())
            {
                //Fetch the next batch of UI interaction
                ProcessEvents();
                
                //Clear the screen and draw the framerate
                ClearScreen(Color.White);
                DrawFramerate(0,0);
                
                //Draw onto the screen
                RefreshScreen(60);
            }
        }
    }
}