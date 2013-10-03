//----------------------------------------------------------------------------
// PointerWrapper.m
//----------------------------------------------------------------------------
//
//  Contains code used by the SwinGame resources. used by SGWrapperGen
//
//----------------------------------------------------------------------------

// The ptrRegistry is responsible for maintaining copies of all wrapped SwinGame pointers.
#import <Foundation/NSValue.h>
#import "PointerManager.h"
#import "SGSDK.h"

static NSMutableDictionary *_ptrRegister;

void removeObject(void *ptr)
{
    id key = [NSValue valueWithPointer:ptr];
    
    NSLog(@"Freeing '%p'-'%@'", ptr, key);
    
    id <PointerWrapper> obj = [_ptrRegister objectForKey:key];
    if (obj != nil)
    {
        [obj releasePointer];
        [_ptrRegister removeObjectForKey: key];
    }
}

@implementation PointerManager : NSObject

+ (void)initialize
{
    //NSLog(@"Created register");
    _ptrRegister = [[NSMutableDictionary alloc] initWithCapacity: 100000];
    sg_Resources_RegisterFreeNotifier(removeObject);
}

+ (void)registerObject:(id)obj withKey:(void *)key
{
    [_ptrRegister setObject:obj forKey:[NSValue valueWithPointer:key]];
}

+ (id)objectForKey:(void *)key
{
    return [_ptrRegister objectForKey:[NSValue valueWithPointer:key]];
}

@end