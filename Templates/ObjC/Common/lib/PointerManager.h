//----------------------------------------------------------------------------
// PointerManager.h
//----------------------------------------------------------------------------
//
//  Contains code used by the SwinGame resources. used by SGWrapperGen
//
//----------------------------------------------------------------------------

#import <Foundation/NSDictionary.h>
#import <Foundation/NSObject.h>

@interface PointerManager : NSObject

+ (void)initialize;
+ (void)registerObject:(id)obj withKey:(void *)key;

+ (id)objectForKey:(void *)key;
    
@end

@protocol PointerWrapper
- (void)releasePointer;
@end