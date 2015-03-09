#import <Foundation/Foundation.h>
#import "SwinGame.h"

int main()
{
    // Create a pool to manage autorelease objects
    NSAutoreleasePool *pool = [[NSAutoreleasePool alloc] init];
    
    [SGGraphics openGraphicsWindow:@"Hello World" 
                             width:800
                            height:600];
    [SGGraphics showSwinGameSplashScreen];
    
    while (![SGInput windowCloseRequested])
    {
        //Update game...
        [SGInput processEvents];
        
        //Draw game...
        [SGGraphics clearScreen:ColorWhite];
        [SGText drawFramerateAtX:0 y:0];
        [SGGraphics refreshScreenRestrictFPS: 60];
        
        // Release the autorelease objects now, then recreate pool
        [pool drain];
        pool = [[NSAutoreleasePool alloc] init];
    }
    
    [SGResources releaseAllResources];
    
    [pool drain];
    return 0;
}
