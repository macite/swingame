#import <Foundation/Foundation.h>
#import "SwinGame.h"

int main()
{
    // Create a pool to manage autorelease objects
    NSAutoreleasePool *pool = [[NSAutoreleasePool alloc] init];
    
    [SGAudio openAudio];
    [SGGraphics openGraphicsWindow:@"Hello World" 
                             width:800
                            height:600];
    [SGColors loadDefaultColors];
    [SGGraphics showSwinGameSplashScreen];
    
    while (![SGInput windowCloseRequested])
    {
        //Update game...
        [SGInput processEvents];
        
        //Draw game...
        [SGGraphics clearScreen:ColorWhite];
        [SGText drawFramerateAtX:0 y:0];
        [SGGraphics refreshScreen];
        
        // Release the autorelease objects now, then recreate pool
        [pool drain];
        pool = [[NSAutoreleasePool alloc] init];
    }
    
    [SGAudio closeAudio];
    [SGResources releaseAllResources];
    
    [pool drain];
    return 0;
}
