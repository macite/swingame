#import "SG%(name)s.h"

#import <Foundation/NSInvocation.h>

#import "PointerManager.h"
#import "SGSDK.h"
#import "SwinGame.h"

@implementation SGWrapped%(name)s : SG%(name)s

+ (SGWrapped%(name)s *) %(camel_name)sWithDelegate:(id)del update:(SEL)sel1 andRead:(SEL)sel2
{
    SGWrapped%(name)s *ret = [[SGWrapped%(name)s alloc] init%(name)sWithDelegate:del update:sel1 andRead:sel2];
    [ret autorelease];
    return ret;
}

- (id)init%(name)sWithDelegate:(id)del update:(SEL)sel1 andRead:(SEL)sel2
{
    self = [super init];
    if (self != nil)
    {
        //If self isn't nil then assign pointer.
        call_on_update = sel1;
        call_on_read   = sel2;
        delegate       = del;
        
        [delegate retain];
    }
    return self;
}

- (void) dealloc
{
    [delegate release];
    [super dealloc];
}

- (void) callUpdate
{
    if (delegate == nil || call_on_update == nil) return;
    
    NSMethodSignature *sig = [self methodSignatureForSelector:call_on_update];
    NSInvocation *inv = [NSInvocation invocationWithMethodSignature: sig];
    [inv setArgument:&self atIndex:2]; //first arg after self + _cmd (for call)
    [inv invokeWithTarget: delegate]; //call on the delegate
}

- (void) callRead
{
    if (delegate == nil || call_on_read == nil) return;
    
    NSMethodSignature *sig = [self methodSignatureForSelector:call_on_read];
    NSInvocation *inv = [NSInvocation invocationWithMethodSignature: sig];
    [inv invokeWithTarget: delegate]; //call on the delegate
    
    SG%(name)s *ret;
    [inv getReturnValue: &ret];
    data = ret->data;
}

%(wrapped_property_bodies)s

@end


@implementation SG%(name)s : NSObject

//
// Update the %(name)s objects in the NSArray arr from the array pointed to by firstPtr.
// This is used to restore data to objects after calling a SwinGame method.
//
+ (void) update%(name)ssIn:(NSArray *)arr fromDataIn:(%(name_lower)s *)firstPtr size:(int)sz
{
    int i;
    SG%(name)s *current;
    
    for (i = 0; i < [arr count]; i++)
    {
        current = (SG%(name)s *)[arr objectAtIndex: i];
        [current setData: *(firstPtr + i)];
    }
}

+ (NSArray *) arrayOf%(name)ss:(%(name_lower)s *)firstPtr size:(int)sz
{
    NSMutableArray *result = [[NSMutableArray alloc] initWithCapacity:sz];
    int i;
    SG%(name)s *obj;
    
    for (i = 0; i < sz; i++)
    {
        obj = [[SG%(name)s alloc] initWith%(name)s: *(firstPtr + i)];
        [result addObject: obj];
        [obj release];
    }
    
    return [result autorelease];
}

+ (SG%(name)s *) %(camel_name)sForData: (%(name_lower)s)dat
{
    SG%(name)s *ret = [[SG%(name)s alloc] initWith%(name)s: dat];
    [ret autorelease];
    return ret;
}

+ (void) get%(name)ss:(%(name_lower)s *)firstPtr fromArray:(const NSArray *)arr maxSize:(int)sz
{
    int i, count = [arr count];
    count = count <= sz ? count: sz; //get min of count and sz
    
    for (i = 0; i < count; i++)
    {
        *(firstPtr + i) = [((SG%(name)s *)[arr objectAtIndex: i]) data];
    }
}

- (SG%(name)s *)initWith%(name)s:(%(name_lower)s)dat
{
    //Assign super's initialised value to the self pointer
    self = [super init];
    if (self != nil)
    {
        //If self isn't nil then assign pointer.
        data = dat;
    }
    return self;
}

- (%(name_lower)s) data
{
    return data;
}

- (void) setData:(%(name_lower)s)dat
{
    data = dat;
}

%(static_method_bodies)s

%(property_synthesizes)s

%(init_bodys)s

%(method_bodies)s

@end
