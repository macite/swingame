//----------------------------------------------------------------------------
// PointerWrapper.cs
//----------------------------------------------------------------------------
//
//  Contains code used by the SwinGame resources. used by SGWrapperGen
//
//----------------------------------------------------------------------------
using System;
using System.Collections.Generic;
using System.Text;
using System.Runtime.InteropServices;

namespace SwinGameSDK
{
    //internal delegate void FreeDelegate(IntPtr toFree);
    [System.ComponentModel.EditorBrowsable(System.ComponentModel.EditorBrowsableState.Never)]
    internal enum PtrKind
    {
        Animation,
        AnimationScript,
        Bitmap,
        Character,
        Font,
        SoundEffect,
        Music,
        Map,
        Sprite,
        Timer,
        Shape,
        ShapePrototype,
        Panel,                     
        Region,               
        GUIRadioGroup,        
        GUIList,          
        GUICheckbox,          
        GUITextbox,           
        GUILabel,
        Connection,
        ServerSocket,
        ArduinoDevice,
        Window,
        Message,
        HttpRequest,
        Copy,
		
    }
    
    /// [UnmanagedFunctionPointerAttribute(CallingConvention.Cdecl)]
    /// internal delegate PointerWrapper Creater(IntPtr pointer);
    
    /// <summary>
    /// Wraps a pointer to a SwinGame resource
    /// </summary>
    [System.ComponentModel.EditorBrowsable(System.ComponentModel.EditorBrowsableState.Never)]
    public abstract class PointerWrapper : IDisposable
    {
        /// <summary>
        /// The ptrRegistry is responsible for maintaining copies of all wrapped SwinGame pointers.
        /// </summary>
        protected static readonly Dictionary<IntPtr, PointerWrapper> _ptrRegister;
        
        [System.ComponentModel.EditorBrowsable(System.ComponentModel.EditorBrowsableState.Never)]
        internal static void Remove(IntPtr ptr)
        {
            if (_ptrRegister.ContainsKey(ptr))
            {
                _ptrRegister.Remove(ptr);
            }
        }
        
        private static FreeNotifier _RemoveMethod = PointerWrapper.Remove;
        
        static PointerWrapper()
        {
            //Register Remove with SwinGame
            //Console.WriteLine("Registering");
            _ptrRegister = new Dictionary<IntPtr, PointerWrapper>();
            sgLibrary.sg_Resources_RegisterFreeNotifier(_RemoveMethod);
        }
        
        /// <summary>
        /// "Super Dodgy" (but correct) work around for the fact that C# has no unload methods for classes.
        /// </summary>
        [System.ComponentModel.EditorBrowsable(System.ComponentModel.EditorBrowsableState.Never)]
        internal class ReleaserClass
        {
            ~ReleaserClass()
            {
                //Console.WriteLine("Deregistering");
                sgLibrary.sg_Resources_RegisterFreeNotifier(null);                
            }
        }
        
        [System.ComponentModel.EditorBrowsable(System.ComponentModel.EditorBrowsableState.Never)]
        internal static ReleaserClass releaser = new ReleaserClass();
        
        private PtrKind _Kind;
        [System.ComponentModel.EditorBrowsable(System.ComponentModel.EditorBrowsableState.Never)]
        protected internal IntPtr Pointer;
        
        [System.ComponentModel.EditorBrowsable(System.ComponentModel.EditorBrowsableState.Never)]
        protected internal abstract void DoFree();
        
        [System.Diagnostics.DebuggerNonUserCode(), System.Diagnostics.DebuggerStepThrough(), System.ComponentModel.EditorBrowsable(System.ComponentModel.EditorBrowsableState.Never)]
        internal PointerWrapper(IntPtr ptr, PtrKind kind)
        {
            if (PointerWrapper._ptrRegister.ContainsKey(ptr)) throw new SwinGameException("Error managing resources.");
            PointerWrapper._ptrRegister[ptr] = this;
            Pointer = ptr;
            _Kind = kind;
        }
        
        [System.Diagnostics.DebuggerNonUserCode(), System.Diagnostics.DebuggerStepThrough(),System.ComponentModel.EditorBrowsable(System.ComponentModel.EditorBrowsableState.Never)]
        public static implicit operator IntPtr(PointerWrapper p)
        {
            return p.Pointer;
        }
        
        #region IDisposable Members
        
        /// <summary>
        /// Clean up the native resources used by this resource.
        /// </summary>
        [System.Diagnostics.DebuggerNonUserCode(), System.Diagnostics.DebuggerStepThrough()]
        public void Dispose()
        {
            if (_Kind != PtrKind.Copy)
            {
              DoFree();
            }
        }
        
        #endregion
        
        /// <summary>
        /// Returns a string representation of the resource. This is in the format
        /// "Type @address".
        /// </summary>
        [System.Diagnostics.DebuggerNonUserCode(), System.Diagnostics.DebuggerStepThrough()]
        public override String ToString()
        {
            return String.Format("{0} @{1:x}", _Kind, Pointer);
        }
        
        /// <summary>
        /// Determines if the PointerWrappers is equal to the passed in object.
        /// </summary>
        [System.Diagnostics.DebuggerNonUserCode(), System.Diagnostics.DebuggerStepThrough()]
        public override bool Equals(object other)
        {
            if (other == null) return this.Pointer == IntPtr.Zero;
            if (other is PointerWrapper) return this.Pointer == ((PointerWrapper)other).Pointer;
            else if (other is IntPtr) return this.Pointer == ((IntPtr)other);
            else return false;
        }
        
        /// <summary>
        /// Returns the hash code of the PointerWrapper based on what it points to.
        /// </summary>
        [System.Diagnostics.DebuggerNonUserCode(), System.Diagnostics.DebuggerStepThrough()]
        public override int GetHashCode()
        {
            return this.Pointer.GetHashCode();
        }
        
        /// <summary>
        /// Determines if two PointerWrappers are equal.
        /// </summary>
        [System.Diagnostics.DebuggerNonUserCode(), System.Diagnostics.DebuggerStepThrough()]
        public static bool operator ==(PointerWrapper pw1, PointerWrapper pw2)
        {
            if ((object)pw1 == null && (object)pw2 == null) return true;
            if ((object)pw1 == null || (object)pw2 == null) return false;
            return pw1.Pointer == pw2.Pointer;
        }
        
        /// <summary>
        /// Determines if two PointerWrappers are not equal.
        /// </summary>
        [System.Diagnostics.DebuggerNonUserCode(), System.Diagnostics.DebuggerStepThrough()]
        public static bool operator !=(PointerWrapper pw1, PointerWrapper pw2)
        {
            if ((object)pw1 == null && (object)pw2 == null) return false;
            if ((object)pw1 == null || (object)pw2 == null) return true;
            return pw1.Pointer != pw2.Pointer;
        }
    }

    /// <summary>
    /// Wraps a pointer to a SwinGame resource
    /// </summary>
    [System.ComponentModel.EditorBrowsable(System.ComponentModel.EditorBrowsableState.Never)]
    public abstract class NoFreePointerWrapper : PointerWrapper
    {
		internal NoFreePointerWrapper(IntPtr ptr, PtrKind kind): base(ptr, kind)
	    {
	    }
		
		protected internal override void DoFree()
		{}
	}
}
