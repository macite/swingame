#import "SGObjcUtils.h"
#import "SGTriangle.h"

#import <Foundation/NSValue.h>

#import <string.h>
#import <stdlib.h>

@implementation NSString (SGStringUtils)

+ (void) getStrings:(char **)firstPtr fromArray:(NSArray *)in_data maxSize:(int)sz;
{
    int i;
    int count = [in_data count];
    count = count <= sz ? count : sz; //get min of sz and count
    for ( i = 0; i < count; i++ ) 
    {
        NSString *str = [in_data objectAtIndex:i];
        strcpy(*(firstPtr + i), [str cStringUsingEncoding:NSASCIIStringEncoding]);
    }
}

+ (NSArray *) arrayOfStrings:(char **)firstPtr size:(int)sz
{
    NSMutableArray *result = [[NSMutableArray alloc] initWithCapacity:sz];
    int i;
    NSString *obj;
    
    for (i = 0; i < sz; i++)
    {
        obj = [[[NSString alloc] initWithCString:*(firstPtr + i) encoding:NSASCIIStringEncoding] autorelease]; //obj is autorelease...
        [result addObject: obj];
    }
    
    return [result autorelease];
}

@end

@implementation SGObjcUtils : NSObject

+ (NSArray *) arrayOfIntegers:(int *)firstPtr size:(int)sz
{
    NSMutableArray *result = [[NSMutableArray alloc] initWithCapacity:sz];
    int i;
    NSNumber *obj;
    
    for (i = 0; i < sz; i++)
    {
        obj = [NSNumber numberWithInt: *(firstPtr + i)]; //obj is autorelease...
        [result addObject: obj];
    }
    
    return [result autorelease];
}

+ (void) getIntegers:(int *)firstPtr fromArray:(NSArray *)in_data maxSize:(int)sz
{
    int i;
    int count = [in_data count];
    count = count <= sz ? count : sz; //get min of sz and count
    for ( i = 0; i < count; i++ ) 
    {
        NSNumber *num = [in_data objectAtIndex:i];
        *(firstPtr + i) = [num intValue];
    }
}

@end

@implementation SGStringBufferManager : NSObject

- (id) initWithBuffer:(char **)buf size:(int)sz
{
    self = [super init];
    if (self != nil)
    {
        //Allocate pointers on heap
        self->buffer = malloc(sizeof(char*) * sz);
        self->size = sz;
        
        int i;
        for(i = 0; i < sz; i++) 
        {
            buffer[i] = malloc(sizeof(char) * 2048);
            //use same pointer in the passed in buffer... allowing us to free here
            buf[i] = buffer[i];
        }
    }
    return self;
}

-(void) dealloc
{
    int i;
    for(i = 0; i < size; i++) 
    {
        free(buffer[i]);
    }
    free(buffer);
    size = 0;
    
    [super dealloc];
}

+ (id) stringBufferManagerFor:(char **)buffer size:(int)sz
{
    return [[[SGStringBufferManager alloc] initWithBuffer:buffer size:sz] autorelease];
}

@end

