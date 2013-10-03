/// <summary> %(doc)s </summary>
public class %(name)s : NoFreePointerWrapper
{
    /// <summary> Internal constructor used by SwinGame to create the %(name)s resource </summary>
    [System.ComponentModel.EditorBrowsable(System.ComponentModel.EditorBrowsableState.Never)]
    internal %(name)s(IntPtr pointer) : base (pointer, PtrKind.%(name)s) {}
    
    /// <summary> Internal method to call constructor via a delegate. </summary>
    [System.ComponentModel.EditorBrowsable(System.ComponentModel.EditorBrowsableState.Never)]
    internal static %(name)s Create(IntPtr ptr)
    {
        if (ptr == IntPtr.Zero) return null;
        
        if (_ptrRegister.ContainsKey(ptr)) return _ptrRegister[ptr] as %(name)s;
        return new %(name)s(ptr);
    }
