#import <Foundation/NSObject.h>
#import <Foundation/NSString.h>
#import <Foundation/NSArray.h>

#import "Types.h"
#import "SGTypes.h"
#import "PointerManager.h"
%(imports)s

@interface SG%(name)s : NSObject <PointerWrapper>
{
@package
    %(name_lower)s pointer;
}

+ (NSArray *) arrayOf%(name)ss:(%(name_lower)s *)firstPtr size:(int)sz;
+ (void) get%(name)ss:(%(name_lower)s *)firstPtr fromArray:(NSArray *)in_data maxSize:(int)sz;
+ (id)createWithId:(%(name_lower)s)ptr;
%(static_method_headers)s
- (id)initWithId:(%(name_lower)s)ptr; 
%(init_headers)s

%(property_headers)s

%(method_headers)s
@end