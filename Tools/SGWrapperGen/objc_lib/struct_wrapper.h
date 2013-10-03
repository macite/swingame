#import <Foundation/NSObject.h>
#import <Foundation/NSString.h>
#import <Foundation/NSArray.h>

#import "Types.h"
#import "SGTypes.h"

@interface SG%(name)s : NSObject
{
@package
    %(name_lower)s data;
}

+ (void) update%(name)ssIn:(NSArray *)arr fromDataIn:(%(name_lower)s *)firstPtr size:(int)sz;
+ (NSArray *) arrayOf%(name)ss:(%(name_lower)s *)firstPtr size:(int)sz;
+ (void) get%(name)ss:(%(name_lower)s *)firstPtr fromArray:(const NSArray *)arr maxSize:(int)sz;

+ (SG%(name)s *) %(camel_name)sForData: (%(name_lower)s)dat;

- (SG%(name)s *)initWith%(name)s:(%(name_lower)s)dat;

- (%(name_lower)s) data;
- (void) setData:(%(name_lower)s)dat;

%(static_method_headers)s

%(init_headers)s

%(property_headers)s

%(method_headers)s

@end

@interface SGWrapped%(name)s : SG%(name)s
{
@package
    id       delegate;
    SEL      call_on_update;
    SEL      call_on_read;
}

+ (SGWrapped%(name)s *) %(camel_name)sWithDelegate:(id)del update:(SEL)sel1 andRead:(SEL)sel2;
- (id) init%(name)sWithDelegate:(id)del update:(SEL)sel1 andRead:(SEL)sel2;

@end
