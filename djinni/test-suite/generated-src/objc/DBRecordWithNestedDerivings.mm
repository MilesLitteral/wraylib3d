// AUTOGENERATED FILE - DO NOT MODIFY!
// This file generated by Djinni from derivings.djinni

#import "DBRecordWithNestedDerivings.h"


@implementation DBRecordWithNestedDerivings

- (nonnull instancetype)initWithKey:(int32_t)key
                                rec:(nonnull DBRecordWithDerivings *)rec
{
    if (self = [super init]) {
        _key = key;
        _rec = rec;
    }
    return self;
}

+ (nonnull instancetype)recordWithNestedDerivingsWithKey:(int32_t)key
                                                     rec:(nonnull DBRecordWithDerivings *)rec
{
    return [(DBRecordWithNestedDerivings*)[self alloc] initWithKey:key
                                                               rec:rec];
}

- (BOOL)isEqual:(id)other
{
    if (![other isKindOfClass:[DBRecordWithNestedDerivings class]]) {
        return NO;
    }
    DBRecordWithNestedDerivings *typedOther = (DBRecordWithNestedDerivings *)other;
    return self.key == typedOther.key &&
            [self.rec isEqual:typedOther.rec];
}

- (NSUInteger)hash
{
    return NSStringFromClass([self class]).hash ^
            (NSUInteger)self.key ^
            self.rec.hash;
}

- (NSComparisonResult)compare:(DBRecordWithNestedDerivings *)other
{
    NSComparisonResult tempResult;
    if (self.key < other.key) {
        tempResult = NSOrderedAscending;
    } else if (self.key > other.key) {
        tempResult = NSOrderedDescending;
    } else {
        tempResult = NSOrderedSame;
    }
    if (tempResult != NSOrderedSame) {
        return tempResult;
    }
    tempResult = [self.rec compare:other.rec];
    if (tempResult != NSOrderedSame) {
        return tempResult;
    }
    return NSOrderedSame;
}

- (NSString *)description
{
    return [NSString stringWithFormat:@"<%@ %p key:%@ rec:%@>", self.class, (void *)self, @(self.key), self.rec];
}

@end