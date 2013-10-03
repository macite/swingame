#import <Foundation/NSObject.h>

#import "SGColors.h"
#import "Colors.h"

@implementation SGColors: NSObject

+ (void)loadDefaultColors
{
    load_default_colors();
}

@end
