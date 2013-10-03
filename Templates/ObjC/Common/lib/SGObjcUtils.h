#import <Foundation/NSObject.h>
#import <Foundation/NSString.h>
#import <Foundation/NSArray.h>

#import "Types.h"
#import "SGTypes.h"

#import "SGTriangle.h"

@interface NSString (SGStringUtils)

+ (void) getStrings:(char **)firstPtr fromArray:(NSArray *)in_data maxSize:(int)sz;
+ (NSArray *) arrayOfStrings:(char **)firstPtr size:(int)sz;

@end

@interface SGObjcUtils : NSObject

+ (NSArray *) arrayOfIntegers:(int *)firstPtr size:(int)sz;
+ (void) getIntegers:(int *)firstPtr fromArray:(NSArray *)in_data maxSize:(int)sz;

@end

@interface SGStringBufferManager : NSObject
{
@protected
    char** buffer;
    int size;
}


+ (id) stringBufferManagerFor:(char **)buffer size:(int)sz;

@end