#include <jni.h>
#include <SGSDK/SGSDK.h>

int main(int argc, char **argv)
{
    sg_Resources_SetAppPathWithExe(argv[0], -1);
    
    JavaVM *vm;
    JNIEnv *env;
    JavaVMInitArgs vm_args;
    JavaVMOption options[4];
    int res;
    jclass stringClass;
    jobjectArray args;
    
    options[0].optionString = "-Djava.compiler=NONE";           /* disable JIT */
    options[1].optionString = "-Djava.class.path=.";            /* user classes */
    options[2].optionString = "-Djava.library.path=.";          /* set native library path */
    options[3].optionString = "-verbose:jni";                   /* print JNI-related messages */
    
    vm_args.version = JNI_VERSION_1_2;
    vm_args.options = options;
    vm_args.nOptions = 4;
    vm_args.ignoreUnrecognized = -1;
    
    res = JNI_CreateJavaVM(&vm, (void **)&env, &vm_args);
    if (res < 0) return -1;
    
    jclass  main_class = (*env)->FindClass(env, "mygame/GameMain");
    jmethodID main_method = (*env)->GetStaticMethodID(env, main_class, "main", "([Ljava/lang/String;)V");
    
    stringClass = (*env)->FindClass(env, "java/lang/String");
    args = (*env)->NewObjectArray(env, 0, stringClass, NULL);
    
    (*env)->CallStaticVoidMethod(env, main_class, main_method, args);
    (*vm)->DestroyJavaVM(vm);
    
    return 1;
}



// 127A Canterbury Rd