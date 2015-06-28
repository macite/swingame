// This file is part of Eigen, a lightweight C++ template library
// for linear algebra.
//
// Copyright (C) 2011 Benoit Jacob <jacob.benoit.1@gmail.com>
// Copyright (C) 2011-2014 Gael Guennebaud <gael.guennebaud@inria.fr>
// Copyright (C) 2011-2012 Jitse Niesen <jitse@maths.leeds.ac.uk>
//
// This Source Code Form is subject to the terms of the Mozilla
// Public License v. 2.0. If a copy of the MPL was not distributed
// with this file, You can obtain one at http://mozilla.org/MPL/2.0/.

#ifndef EIGEN_ASSIGN_EVALUATOR_H
#define EIGEN_ASSIGN_EVALUATOR_H

namespace Eigen {

// This implementation is based on Assign.h

namespace internal {
  
/***************************************************************************
* Part 1 : the logic deciding a strategy for traversal and unrolling       *
***************************************************************************/

// copy_using_evaluator_traits is based on assign_traits

template <typename DstEvaluator, typename SrcEvaluator, typename AssignFunc>
struct copy_using_evaluator_traits
{
  typedef typename DstEvaluator::XprType Dst;
  
  enum {
    DstFlags = DstEvaluator::Flags,
    SrcFlags = SrcEvaluator::Flags
  };
  
public:
  enum {
    DstIsAligned = DstFlags & AlignedBit,
    DstHasDirectAccess = DstFlags & DirectAccessBit,
    SrcIsAligned = SrcFlags & AlignedBit,
    JointAlignment = bool(DstIsAligned) && bool(SrcIsAligned) ? Aligned : Unaligned
  };

private:
  enum {
    InnerSize = int(Dst::IsVectorAtCompileTime) ? int(Dst::SizeAtCompileTime)
              : int(DstFlags)&RowMajorBit ? int(Dst::ColsAtCompileTime)
              : int(Dst::RowsAtCompileTime),
    InnerMaxSize = int(Dst::IsVectorAtCompileTime) ? int(Dst::MaxSizeAtCompileTime)
              : int(DstFlags)&RowMajorBit ? int(Dst::MaxColsAtCompileTime)
              : int(Dst::MaxRowsAtCompileTime),
    MaxSizeAtCompileTime = Dst::SizeAtCompileTime,
    PacketSize = packet_traits<typename Dst::Scalar>::size
  };

  enum {
    DstIsRowMajor = DstFlags&RowMajorBit,
    SrcIsRowMajor = SrcFlags&RowMajorBit,
    StorageOrdersAgree = (int(DstIsRowMajor) == int(SrcIsRowMajor)),
    MightVectorize = StorageOrdersAgree
                  && (int(DstFlags) & int(SrcFlags) & ActualPacketAccessBit)
                  && (functor_traits<AssignFunc>::PacketAccess),
    MayInnerVectorize  = MightVectorize && int(InnerSize)!=Dynamic && int(InnerSize)%int(PacketSize)==0
                       && int(DstIsAligned) && int(SrcIsAligned),
    MayLinearize = StorageOrdersAgree && (int(DstFlags) & int(SrcFlags) & LinearAccessBit),
    MayLinearVectorize = MightVectorize && MayLinearize && DstHasDirectAccess
                       && (DstIsAligned || MaxSizeAtCompileTime == Dynamic),
      /* If the destination isn't aligned, we have to do runtime checks and we don't unroll,
         so it's only good for large enough sizes. */
    MaySliceVectorize  = MightVectorize && DstHasDirectAccess
                       && (int(InnerMaxSize)==Dynamic || int(InnerMaxSize)>=3*PacketSize)
      /* slice vectorization can be slow, so we only want it if the slices are big, which is
         indicated by InnerMaxSize rather than InnerSize, think of the case of a dynamic block
         in a fixed-size matrix */
  };

public:
  enum {
    Traversal = int(MayInnerVectorize)   ? int(InnerVectorizedTraversal)
              : int(MayLinearVectorize)  ? int(LinearVectorizedTraversal)
              : int(MaySliceVectorize)   ? int(SliceVectorizedTraversal)
              : int(MayLinearize)        ? int(LinearTraversal)
                                         : int(DefaultTraversal),
    Vectorized = int(Traversal) == InnerVectorizedTraversal
              || int(Traversal) == LinearVectorizedTraversal
              || int(Traversal) == SliceVectorizedTraversal
  };

private:
  enum {
    UnrollingLimit      = EIGEN_UNROLLING_LIMIT * (Vectorized ? int(PacketSize) : 1),
    MayUnrollCompletely = int(Dst::SizeAtCompileTime) != Dynamic
                       && int(SrcEvaluator::CoeffReadCost) != Dynamic
                       && int(Dst::SizeAtCompileTime) * int(SrcEvaluator::CoeffReadCost) <= int(UnrollingLimit),
    MayUnrollInner      = int(InnerSize) != Dynamic
                       && int(SrcEvaluator::CoeffReadCost) != Dynamic
                       && int(InnerSize) * int(SrcEvaluator::CoeffReadCost) <= int(UnrollingLimit)
  };

public:
  enum {
    Unrolling = (int(Traversal) == int(InnerVectorizedTraversal) || int(Traversal) == int(DefaultTraversal))
                ? (
                    int(MayUnrollCompletely) ? int(CompleteUnrolling)
                  : int(MayUnrollInner)      ? int(InnerUnrolling)
                                             : int(NoUnrolling)
                  )
              : int(Traversal) == int(LinearVectorizedTraversal)
                ? ( bool(MayUnrollCompletely) && bool(DstIsAligned) ? int(CompleteUnrolling) 
                                                                    : int(NoUnrolling) )
              : int(Traversal) == int(LinearTraversal)
                ? ( bool(MayUnrollCompletely) ? int(CompleteUnrolling) 
                                              : int(NoUnrolling) )
              : int(NoUnrolling)
  };

#ifdef EIGEN_DEBUG_ASSIGN
  static void debug()
  {
    std::cerr << "DstXpr: " << typeid(typename DstEvaluator::XprType).name() << std::endl;
    std::cerr << "SrcXpr: " << typeid(typename SrcEvaluator::XprType).name() << std::endl;
    std::cerr.setf(std::ios::hex, std::ios::basefield);
    EIGEN_DEBUG_VAR(DstFlags)
    EIGEN_DEBUG_VAR(SrcFlags)
    std::cerr.unsetf(std::ios::hex);
    EIGEN_DEBUG_VAR(DstIsAligned)
    EIGEN_DEBUG_VAR(SrcIsAligned)
    EIGEN_DEBUG_VAR(JointAlignment)
    EIGEN_DEBUG_VAR(InnerSize)
    EIGEN_DEBUG_VAR(InnerMaxSize)
    EIGEN_DEBUG_VAR(PacketSize)
    EIGEN_DEBUG_VAR(StorageOrdersAgree)
    EIGEN_DEBUG_VAR(MightVectorize)
    EIGEN_DEBUG_VAR(MayLinearize)
    EIGEN_DEBUG_VAR(MayInnerVectorize)
    EIGEN_DEBUG_VAR(MayLinearVectorize)
    EIGEN_DEBUG_VAR(MaySliceVectorize)
    EIGEN_DEBUG_VAR(Traversal)
    EIGEN_DEBUG_VAR(UnrollingLimit)
    EIGEN_DEBUG_VAR(MayUnrollCompletely)
    EIGEN_DEBUG_VAR(MayUnrollInner)
    EIGEN_DEBUG_VAR(Unrolling)
    std::cerr << std::endl;
  }
#endif
};

/***************************************************************************
* Part 2 : meta-unrollers
***************************************************************************/

/************************
*** Default traversal ***
************************/

template<typename Kernel, int Index, int Stop>
struct copy_using_evaluator_DefaultTraversal_CompleteUnrolling
{
  // FIXME: this is not very clean, perhaps this information should be provided by the kernel?
  typedef typename Kernel::DstEvaluatorType DstEvaluatorType;
  typedef typename DstEvaluatorType::XprType DstXprType;
  
  enum {
    outer = Index / DstXprType::InnerSizeAtCompileTime,
    inner = Index % DstXprType::InnerSizeAtCompileTime
  };

  EIGEN_DEVICE_FUNC static EIGEN_STRONG_INLINE void run(Kernel &kernel)
  {
    kernel.assignCoeffByOuterInner(outer, inner);
    copy_using_evaluator_DefaultTraversal_CompleteUnrolling<Kernel, Index+1, Stop>::run(kernel);
  }
};

template<typename Kernel, int Stop>
struct copy_using_evaluator_DefaultTraversal_CompleteUnrolling<Kernel, Stop, Stop>
{
  EIGEN_DEVICE_FUNC static EIGEN_STRONG_INLINE void run(Kernel&) { }
};

template<typename Kernel, int Index_, int Stop>
struct copy_using_evaluator_DefaultTraversal_InnerUnrolling
{
  EIGEN_DEVICE_FUNC static EIGEN_STRONG_INLINE void run(Kernel &kernel, Index outer)
  {
    kernel.assignCoeffByOuterInner(outer, Index_);
    copy_using_evaluator_DefaultTraversal_InnerUnrolling<Kernel, Index_+1, Stop>::run(kernel, outer);
  }
};

template<typename Kernel, int Stop>
struct copy_using_evaluator_DefaultTraversal_InnerUnrolling<Kernel, Stop, Stop>
{
  EIGEN_DEVICE_FUNC static EIGEN_STRONG_INLINE void run(Kernel&, Index) { }
};

/***********************
*** Linear traversal ***
***********************/

template<typename Kernel, int Index, int Stop>
struct copy_using_evaluator_LinearTraversal_CompleteUnrolling
{
  EIGEN_DEVICE_FUNC static EIGEN_STRONG_INLINE void run(Kernel& kernel)
  {
    kernel.assignCoeff(Index);
    copy_using_evaluator_LinearTraversal_CompleteUnrolling<Kernel, Index+1, Stop>::run(kernel);
  }
};

template<typename Kernel, int Stop>
struct copy_using_evaluator_LinearTraversal_CompleteUnrolling<Kernel, Stop, Stop>
{
  EIGEN_DEVICE_FUNC static EIGEN_STRONG_INLINE void run(Kernel&) { }
};

/**************************
*** Inner vectorization ***
**************************/

template<typename Kernel, int Index, int Stop>
struct copy_using_evaluator_innervec_CompleteUnrolling
{
  // FIXME: this is not very clean, perhaps this information should be provided by the kernel?
  typedef typename Kernel::DstEvaluatorType DstEvaluatorType;
  typedef typename DstEvaluatorType::XprType DstXprType;
  
  enum {
    outer = Index / DstXprType::InnerSizeAtCompileTime,
    inner = Index % DstXprType::InnerSizeAtCompileTime,
    JointAlignment = Kernel::AssignmentTraits::JointAlignment
  };

  EIGEN_DEVICE_FUNC static EIGEN_STRONG_INLINE void run(Kernel &kernel)
  {
    kernel.template assignPacketByOuterInner<Aligned, JointAlignment>(outer, inner);
    enum { NextIndex = Index + packet_traits<typename DstXprType::Scalar>::size };
    copy_using_evaluator_innervec_CompleteUnrolling<Kernel, NextIndex, Stop>::run(kernel);
  }
};

template<typename Kernel, int Stop>
struct copy_using_evaluator_innervec_CompleteUnrolling<Kernel, Stop, Stop>
{
  EIGEN_DEVICE_FUNC static EIGEN_STRONG_INLINE void run(Kernel&) { }
};

template<typename Kernel, int Index_, int Stop>
struct copy_using_evaluator_innervec_InnerUnrolling
{
  EIGEN_DEVICE_FUNC static EIGEN_STRONG_INLINE void run(Kernel &kernel, Index outer)
  {
    kernel.template assignPacketByOuterInner<Aligned, Aligned>(outer, Index_);
    enum { NextIndex = Index_ + packet_traits<typename Kernel::Scalar>::size };
    copy_using_evaluator_innervec_InnerUnrolling<Kernel, NextIndex, Stop>::run(kernel, outer);
  }
};

template<typename Kernel, int Stop>
struct copy_using_evaluator_innervec_InnerUnrolling<Kernel, Stop, Stop>
{
  EIGEN_DEVICE_FUNC static EIGEN_STRONG_INLINE void run(Kernel &, Index) { }
};

/***************************************************************************
* Part 3 : implementation of all cases
***************************************************************************/

// dense_assignment_loop is based on assign_impl

template<typename Kernel,
         int Traversal = Kernel::AssignmentTraits::Traversal,
         int Unrolling = Kernel::AssignmentTraits::Unrolling>
struct dense_assignment_loop;

/************************
*** Default traversal ***
************************/

template<typename Kernel>
struct dense_assignment_loop<Kernel, DefaultTraversal, NoUnrolling>
{
  EIGEN_DEVICE_FUNC static void run(Kernel &kernel)
  {
    for(Index outer = 0; outer < kernel.outerSize(); ++outer) {
      for(Index inner = 0; inner < kernel.innerSize(); ++inner) {
        kernel.assignCoeffByOuterInner(outer, inner);
      }
    }
  }
};

template<typename Kernel>
struct dense_assignment_loop<Kernel, DefaultTraversal, CompleteUnrolling>
{
  EIGEN_DEVICE_FUNC static EIGEN_STRONG_INLINE void run(Kernel &kernel)
  {
    typedef typename Kernel::DstEvaluatorType::XprType DstXprType;
    copy_using_evaluator_DefaultTraversal_CompleteUnrolling<Kernel, 0, DstXprType::SizeAtCompileTime>::run(kernel);
  }
};

template<typename Kernel>
struct dense_assignment_loop<Kernel, DefaultTraversal, InnerUnrolling>
{
  typedef typename Kernel::StorageIndex StorageIndex;
  EIGEN_DEVICE_FUNC static EIGEN_STRONG_INLINE void run(Kernel &kernel)
  {
    typedef typename Kernel::DstEvaluatorType::XprType DstXprType;

    const Index outerSize = kernel.outerSize();
    for(Index outer = 0; outer < outerSize; ++outer)
      copy_using_evaluator_DefaultTraversal_InnerUnrolling<Kernel, 0, DstXprType::InnerSizeAtCompileTime>::run(kernel, outer);
  }
};

/***************************
*** Linear vectorization ***
***************************/


// The goal of unaligned_dense_assignment_loop is simply to factorize the handling
// of the non vectorizable beginning and ending parts

template <bool IsAligned = false>
struct unaligned_dense_assignment_loop
{
  // if IsAligned = true, then do nothing
  template <typename Kernel>
  EIGEN_DEVICE_FUNC static EIGEN_STRONG_INLINE void run(Kernel&, Index, Index) {}
};

template <>
struct unaligned_dense_assignment_loop<false>
{
  // MSVC must not inline this functions. If it does, it fails to optimize the
  // packet access path.
  // FIXME check which version exhibits this issue
#if EIGEN_COMP_MSVC
  template <typename Kernel>
  static EIGEN_DONT_INLINE void run(Kernel &kernel,
                                    Index start,
                                    Index end)
#else
  template <typename Kernel>
  EIGEN_DEVICE_FUNC static EIGEN_STRONG_INLINE void run(Kernel &kernel,
                                      Index start,
                                      Index end)
#endif
  {
    for (Index index = start; index < end; ++index)
      kernel.assignCoeff(index);
  }
};

template<typename Kernel>
struct dense_assignment_loop<Kernel, LinearVectorizedTraversal, NoUnrolling>
{
  EIGEN_DEVICE_FUNC static EIGEN_STRONG_INLINE void run(Kernel &kernel)
  {
    const Index size = kernel.size();
    typedef packet_traits<typename Kernel::Scalar> PacketTraits;
    enum {
      packetSize = PacketTraits::size,
      dstIsAligned = int(Kernel::AssignmentTraits::DstIsAligned),
      dstAlignment = PacketTraits::AlignedOnScalar ? Aligned : dstIsAligned,
      srcAlignment = Kernel::AssignmentTraits::JointAlignment
    };
    const Index alignedStart = dstIsAligned ? 0 : internal::first_aligned(&kernel.dstEvaluator().coeffRef(0), size);
    const Index alignedEnd = alignedStart + ((size-alignedStart)/packetSize)*packetSize;

    unaligned_dense_assignment_loop<dstIsAligned!=0>::run(kernel, 0, alignedStart);

    for(Index index = alignedStart; index < alignedEnd; index += packetSize)
      kernel.template assignPacket<dstAlignment, srcAlignment>(index);

    unaligned_dense_assignment_loop<>::run(kernel, alignedEnd, size);
  }
};

template<typename Kernel>
struct dense_assignment_loop<Kernel, LinearVectorizedTraversal, CompleteUnrolling>
{
  typedef typename Kernel::StorageIndex StorageIndex;
  EIGEN_DEVICE_FUNC static EIGEN_STRONG_INLINE void run(Kernel &kernel)
  {
    typedef typename Kernel::DstEvaluatorType::XprType DstXprType;
    
    enum { size = DstXprType::SizeAtCompileTime,
           packetSize = packet_traits<typename Kernel::Scalar>::size,
           alignedSize = (size/packetSize)*packetSize };

    copy_using_evaluator_innervec_CompleteUnrolling<Kernel, 0, alignedSize>::run(kernel);
    copy_using_evaluator_DefaultTraversal_CompleteUnrolling<Kernel, alignedSize, size>::run(kernel);
  }
};

/**************************
*** Inner vectorization ***
**************************/

template<typename Kernel>
struct dense_assignment_loop<Kernel, InnerVectorizedTraversal, NoUnrolling>
{
  EIGEN_DEVICE_FUNC static inline void run(Kernel &kernel)
  {
    const Index innerSize = kernel.innerSize();
    const Index outerSize = kernel.outerSize();
    const Index packetSize = packet_traits<typename Kernel::Scalar>::size;
    for(Index outer = 0; outer < outerSize; ++outer)
      for(Index inner = 0; inner < innerSize; inner+=packetSize)
        kernel.template assignPacketByOuterInner<Aligned, Aligned>(outer, inner);
  }
};

template<typename Kernel>
struct dense_assignment_loop<Kernel, InnerVectorizedTraversal, CompleteUnrolling>
{
  EIGEN_DEVICE_FUNC static EIGEN_STRONG_INLINE void run(Kernel &kernel)
  {
    typedef typename Kernel::DstEvaluatorType::XprType DstXprType;
    copy_using_evaluator_innervec_CompleteUnrolling<Kernel, 0, DstXprType::SizeAtCompileTime>::run(kernel);
  }
};

template<typename Kernel>
struct dense_assignment_loop<Kernel, InnerVectorizedTraversal, InnerUnrolling>
{
  typedef typename Kernel::StorageIndex StorageIndex;
  EIGEN_DEVICE_FUNC static EIGEN_STRONG_INLINE void run(Kernel &kernel)
  {
    typedef typename Kernel::DstEvaluatorType::XprType DstXprType;
    const Index outerSize = kernel.outerSize();
    for(Index outer = 0; outer < outerSize; ++outer)
      copy_using_evaluator_innervec_InnerUnrolling<Kernel, 0, DstXprType::InnerSizeAtCompileTime>::run(kernel, outer);
  }
};

/***********************
*** Linear traversal ***
***********************/

template<typename Kernel>
struct dense_assignment_loop<Kernel, LinearTraversal, NoUnrolling>
{
  EIGEN_DEVICE_FUNC static inline void run(Kernel &kernel)
  {
    const Index size = kernel.size();
    for(Index i = 0; i < size; ++i)
      kernel.assignCoeff(i);
  }
};

template<typename Kernel>
struct dense_assignment_loop<Kernel, LinearTraversal, CompleteUnrolling>
{
  EIGEN_DEVICE_FUNC static EIGEN_STRONG_INLINE void run(Kernel &kernel)
  {
    typedef typename Kernel::DstEvaluatorType::XprType DstXprType;
    copy_using_evaluator_LinearTraversal_CompleteUnrolling<Kernel, 0, DstXprType::SizeAtCompileTime>::run(kernel);
  }
};

/**************************
*** Slice vectorization ***
***************************/

template<typename Kernel>
struct dense_assignment_loop<Kernel, SliceVectorizedTraversal, NoUnrolling>
{
  EIGEN_DEVICE_FUNC static inline void run(Kernel &kernel)
  {
    typedef typename Kernel::Scalar Scalar;
    typedef packet_traits<Scalar> PacketTraits;
    enum {
      packetSize = PacketTraits::size,
      alignable = PacketTraits::AlignedOnScalar,
      dstIsAligned = Kernel::AssignmentTraits::DstIsAligned,
      dstAlignment = alignable ? Aligned : int(dstIsAligned)
    };
    const Scalar *dst_ptr = &kernel.dstEvaluator().coeffRef(0,0);
    if((!bool(dstIsAligned)) && (Index(dst_ptr) % sizeof(Scalar))>0)
    {
      // the pointer is not aligend-on scalar, so alignment is not possible
      return dense_assignment_loop<Kernel,DefaultTraversal,NoUnrolling>::run(kernel);
    }
    const Index packetAlignedMask = packetSize - 1;
    const Index innerSize = kernel.innerSize();
    const Index outerSize = kernel.outerSize();
    const Index alignedStep = alignable ? (packetSize - kernel.outerStride() % packetSize) & packetAlignedMask : 0;
    Index alignedStart = ((!alignable) || bool(dstIsAligned)) ? 0 : internal::first_aligned(dst_ptr, innerSize);

    for(Index outer = 0; outer < outerSize; ++outer)
    {
      const Index alignedEnd = alignedStart + ((innerSize-alignedStart) & ~packetAlignedMask);
      // do the non-vectorizable part of the assignment
      for(Index inner = 0; inner<alignedStart ; ++inner)
        kernel.assignCoeffByOuterInner(outer, inner);

      // do the vectorizable part of the assignment
      for(Index inner = alignedStart; inner<alignedEnd; inner+=packetSize)
        kernel.template assignPacketByOuterInner<dstAlignment, Unaligned>(outer, inner);

      // do the non-vectorizable part of the assignment
      for(Index inner = alignedEnd; inner<innerSize ; ++inner)
        kernel.assignCoeffByOuterInner(outer, inner);

      alignedStart = std::min<Index>((alignedStart+alignedStep)%packetSize, innerSize);
    }
  }
};

/***************************************************************************
* Part 4 : Generic dense assignment kernel
***************************************************************************/

// This class generalize the assignment of a coefficient (or packet) from one dense evaluator
// to another dense writable evaluator.
// It is parametrized by the two evaluators, and the actual assignment functor.
// This abstraction level permits to keep the evaluation loops as simple and as generic as possible.
// One can customize the assignment using this generic dense_assignment_kernel with different
// functors, or by completely overloading it, by-passing a functor.
template<typename DstEvaluatorTypeT, typename SrcEvaluatorTypeT, typename Functor, int Version = Specialized>
class generic_dense_assignment_kernel
{
protected:
  typedef typename DstEvaluatorTypeT::XprType DstXprType;
  typedef typename SrcEvaluatorTypeT::XprType SrcXprType;
public:
  
  typedef DstEvaluatorTypeT DstEvaluatorType;
  typedef SrcEvaluatorTypeT SrcEvaluatorType;
  typedef typename DstEvaluatorType::Scalar Scalar;
  typedef typename DstEvaluatorType::StorageIndex StorageIndex;
  typedef copy_using_evaluator_traits<DstEvaluatorTypeT, SrcEvaluatorTypeT, Functor> AssignmentTraits;
  
  
  EIGEN_DEVICE_FUNC generic_dense_assignment_kernel(DstEvaluatorType &dst, const SrcEvaluatorType &src, const Functor &func, DstXprType& dstExpr)
    : m_dst(dst), m_src(src), m_functor(func), m_dstExpr(dstExpr)
  {
    #ifdef EIGEN_DEBUG_ASSIGN
    AssignmentTraits::debug();
    #endif
  }
  
  EIGEN_DEVICE_FUNC Index size() const        { return m_dstExpr.size(); }
  EIGEN_DEVICE_FUNC Index innerSize() const   { return m_dstExpr.innerSize(); }
  EIGEN_DEVICE_FUNC Index outerSize() const   { return m_dstExpr.outerSize(); }
  EIGEN_DEVICE_FUNC Index rows() const        { return m_dstExpr.rows(); }
  EIGEN_DEVICE_FUNC Index cols() const        { return m_dstExpr.cols(); }
  EIGEN_DEVICE_FUNC Index outerStride() const { return m_dstExpr.outerStride(); }
  
  // TODO get rid of this one:
  EIGEN_DEVICE_FUNC DstXprType& dstExpression() const { return m_dstExpr; }
  
  EIGEN_DEVICE_FUNC DstEvaluatorType& dstEvaluator() { return m_dst; }
  EIGEN_DEVICE_FUNC const SrcEvaluatorType& srcEvaluator() const { return m_src; }
  
  /// Assign src(row,col) to dst(row,col) through the assignment functor.
  EIGEN_DEVICE_FUNC void assignCoeff(Index row, Index col)
  {
    m_functor.assignCoeff(m_dst.coeffRef(row,col), m_src.coeff(row,col));
  }
  
  /// \sa assignCoeff(Index,Index)
  EIGEN_DEVICE_FUNC void assignCoeff(Index index)
  {
    m_functor.assignCoeff(m_dst.coeffRef(index), m_src.coeff(index));
  }
  
  /// \sa assignCoeff(Index,Index)
  EIGEN_DEVICE_FUNC void assignCoeffByOuterInner(Index outer, Index inner)
  {
    Index row = rowIndexByOuterInner(outer, inner); 
    Index col = colIndexByOuterInner(outer, inner); 
    assignCoeff(row, col);
  }
  
  
  template<int StoreMode, int LoadMode>
  EIGEN_DEVICE_FUNC void assignPacket(Index row, Index col)
  {
    m_functor.template assignPacket<StoreMode>(&m_dst.coeffRef(row,col), m_src.template packet<LoadMode>(row,col));
  }
  
  template<int StoreMode, int LoadMode>
  EIGEN_DEVICE_FUNC void assignPacket(Index index)
  {
    m_functor.template assignPacket<StoreMode>(&m_dst.coeffRef(index), m_src.template packet<LoadMode>(index));
  }
  
  template<int StoreMode, int LoadMode>
  EIGEN_DEVICE_FUNC void assignPacketByOuterInner(Index outer, Index inner)
  {
    Index row = rowIndexByOuterInner(outer, inner); 
    Index col = colIndexByOuterInner(outer, inner);
    assignPacket<StoreMode,LoadMode>(row, col);
  }
  
  EIGEN_DEVICE_FUNC static Index rowIndexByOuterInner(Index outer, Index inner)
  {
    typedef typename DstEvaluatorType::ExpressionTraits Traits;
    return int(Traits::RowsAtCompileTime) == 1 ? 0
      : int(Traits::ColsAtCompileTime) == 1 ? inner
      : int(DstEvaluatorType::Flags)&RowMajorBit ? outer
      : inner;
  }

  EIGEN_DEVICE_FUNC static Index colIndexByOuterInner(Index outer, Index inner)
  {
    typedef typename DstEvaluatorType::ExpressionTraits Traits;
    return int(Traits::ColsAtCompileTime) == 1 ? 0
      : int(Traits::RowsAtCompileTime) == 1 ? inner
      : int(DstEvaluatorType::Flags)&RowMajorBit ? inner
      : outer;
  }
  
protected:
  DstEvaluatorType& m_dst;
  const SrcEvaluatorType& m_src;
  const Functor &m_functor;
  // TODO find a way to avoid the needs of the original expression
  DstXprType& m_dstExpr;
};

/***************************************************************************
* Part 5 : Entry point for dense rectangular assignment
***************************************************************************/

template<typename DstXprType, typename SrcXprType, typename Functor>
EIGEN_DEVICE_FUNC void call_dense_assignment_loop(const DstXprType& dst, const SrcXprType& src, const Functor &func)
{
  eigen_assert(dst.rows() == src.rows() && dst.cols() == src.cols());
  
  typedef typename evaluator<DstXprType>::type DstEvaluatorType;
  typedef typename evaluator<SrcXprType>::type SrcEvaluatorType;

  DstEvaluatorType dstEvaluator(dst);
  SrcEvaluatorType srcEvaluator(src);
    
  typedef generic_dense_assignment_kernel<DstEvaluatorType,SrcEvaluatorType,Functor> Kernel;
  Kernel kernel(dstEvaluator, srcEvaluator, func, dst.const_cast_derived());
  
  dense_assignment_loop<Kernel>::run(kernel);
}

template<typename DstXprType, typename SrcXprType>
EIGEN_DEVICE_FUNC void call_dense_assignment_loop(const DstXprType& dst, const SrcXprType& src)
{
  call_dense_assignment_loop(dst, src, internal::assign_op<typename DstXprType::Scalar>());
}

/***************************************************************************
* Part 6 : Generic assignment
***************************************************************************/

// Based on the respective shapes of the destination and source,
// the class AssignmentKind determine the kind of assignment mechanism.
// AssignmentKind must define a Kind typedef.
template<typename DstShape, typename SrcShape> struct AssignmentKind;

// Assignement kind defined in this file:
struct Dense2Dense {};
struct EigenBase2EigenBase {};

template<typename,typename> struct AssignmentKind { typedef EigenBase2EigenBase Kind; };
template<> struct AssignmentKind<DenseShape,DenseShape> { typedef Dense2Dense Kind; };
    
// This is the main assignment class
template< typename DstXprType, typename SrcXprType, typename Functor,
          typename Kind = typename AssignmentKind< typename evaluator_traits<DstXprType>::Shape , typename evaluator_traits<SrcXprType>::Shape >::Kind,
          typename Scalar = typename DstXprType::Scalar>
struct Assignment;


// The only purpose of this call_assignment() function is to deal with noalias() / AssumeAliasing and automatic transposition.
// Indeed, I (Gael) think that this concept of AssumeAliasing was a mistake, and it makes thing quite complicated.
// So this intermediate function removes everything related to AssumeAliasing such that Assignment
// does not has to bother about these annoying details.

template<typename Dst, typename Src>
EIGEN_DEVICE_FUNC void call_assignment(Dst& dst, const Src& src)
{
  call_assignment(dst, src, internal::assign_op<typename Dst::Scalar>());
}
template<typename Dst, typename Src>
EIGEN_DEVICE_FUNC void call_assignment(const Dst& dst, const Src& src)
{
  call_assignment(dst, src, internal::assign_op<typename Dst::Scalar>());
}
                     
// Deal with AssumeAliasing
template<typename Dst, typename Src, typename Func>
EIGEN_DEVICE_FUNC void call_assignment(Dst& dst, const Src& src, const Func& func, typename enable_if<evaluator_traits<Src>::AssumeAliasing==1, void*>::type = 0)
{
  typename plain_matrix_type<Src>::type tmp(src);
  call_assignment_no_alias(dst, tmp, func);
}

template<typename Dst, typename Src, typename Func>
EIGEN_DEVICE_FUNC void call_assignment(Dst& dst, const Src& src, const Func& func, typename enable_if<evaluator_traits<Src>::AssumeAliasing==0, void*>::type = 0)
{
  call_assignment_no_alias(dst, src, func);
}

// by-pass AssumeAliasing
// FIXME the const version should probably not be needed
// When there is no aliasing, we require that 'dst' has been properly resized
template<typename Dst, template <typename> class StorageBase, typename Src, typename Func>
EIGEN_DEVICE_FUNC void call_assignment(const NoAlias<Dst,StorageBase>& dst, const Src& src, const Func& func)
{
  call_assignment_no_alias(dst.expression(), src, func);
}
template<typename Dst, template <typename> class StorageBase, typename Src, typename Func>
EIGEN_DEVICE_FUNC void call_assignment(NoAlias<Dst,StorageBase>& dst, const Src& src, const Func& func)
{
  call_assignment_no_alias(dst.expression(), src, func);
}


template<typename Dst, typename Src, typename Func>
EIGEN_DEVICE_FUNC void call_assignment_no_alias(Dst& dst, const Src& src, const Func& func)
{
  enum {
    NeedToTranspose = (  (int(Dst::RowsAtCompileTime) == 1 && int(Src::ColsAtCompileTime) == 1)
                        |   // FIXME | instead of || to please GCC 4.4.0 stupid warning "suggest parentheses around &&".
                                // revert to || as soon as not needed anymore.
                         (int(Dst::ColsAtCompileTime) == 1 && int(Src::RowsAtCompileTime) == 1))
                     && int(Dst::SizeAtCompileTime) != 1
  };

  Index dstRows = NeedToTranspose ? src.cols() : src.rows();
  Index dstCols = NeedToTranspose ? src.rows() : src.cols();
  if((dst.rows()!=dstRows) || (dst.cols()!=dstCols))
    dst.resize(dstRows, dstCols);
  
  typedef typename internal::conditional<NeedToTranspose, Transpose<Dst>, Dst>::type ActualDstTypeCleaned;
  typedef typename internal::conditional<NeedToTranspose, Transpose<Dst>, Dst&>::type ActualDstType;
  ActualDstType actualDst(dst);
  
  // TODO check whether this is the right place to perform these checks:
  EIGEN_STATIC_ASSERT_LVALUE(Dst)
  EIGEN_STATIC_ASSERT_SAME_MATRIX_SIZE(ActualDstTypeCleaned,Src)

  // TODO this line is commented to allow matrix = permutation
  // Actually, the "Scalar" type for a permutation matrix does not really make sense,
  // perhaps it could be void, and EIGEN_CHECK_BINARY_COMPATIBILIY could allow micing void with anything...?
//   EIGEN_CHECK_BINARY_COMPATIBILIY(Func,typename ActualDstTypeCleaned::Scalar,typename Src::Scalar);
  
  Assignment<ActualDstTypeCleaned,Src,Func>::run(actualDst, src, func);
}
template<typename Dst, typename Src>
EIGEN_DEVICE_FUNC void call_assignment_no_alias(Dst& dst, const Src& src)
{
  call_assignment_no_alias(dst, src, internal::assign_op<typename Dst::Scalar>());
}

template<typename Dst, typename Src, typename Func>
EIGEN_DEVICE_FUNC void call_assignment_no_alias_no_transpose(Dst& dst, const Src& src, const Func& func)
{
  Index dstRows = src.rows();
  Index dstCols = src.cols();
  if((dst.rows()!=dstRows) || (dst.cols()!=dstCols))
    dst.resize(dstRows, dstCols);
  
  // TODO check whether this is the right place to perform these checks:
  EIGEN_STATIC_ASSERT_LVALUE(Dst)
  EIGEN_STATIC_ASSERT_SAME_MATRIX_SIZE(Dst,Src)
  
  Assignment<Dst,Src,Func>::run(dst, src, func);
}
template<typename Dst, typename Src>
EIGEN_DEVICE_FUNC void call_assignment_no_alias_no_transpose(Dst& dst, const Src& src)
{
  call_assignment_no_alias_no_transpose(dst, src, internal::assign_op<typename Dst::Scalar>());
}

// forward declaration
template<typename Dst, typename Src> void check_for_aliasing(const Dst &dst, const Src &src);

// Generic Dense to Dense assignment
template< typename DstXprType, typename SrcXprType, typename Functor, typename Scalar>
struct Assignment<DstXprType, SrcXprType, Functor, Dense2Dense, Scalar>
{
  EIGEN_DEVICE_FUNC static void run(DstXprType &dst, const SrcXprType &src, const Functor &func)
  {
    eigen_assert(dst.rows() == src.rows() && dst.cols() == src.cols());
    
#ifndef EIGEN_NO_DEBUG
    internal::check_for_aliasing(dst, src);
#endif
    
    call_dense_assignment_loop(dst, src, func);
  }
};

// Generic assignment through evalTo.
// TODO: not sure we have to keep that one, but it helps porting current code to new evaluator mechanism.
template< typename DstXprType, typename SrcXprType, typename Functor, typename Scalar>
struct Assignment<DstXprType, SrcXprType, Functor, EigenBase2EigenBase, Scalar>
{
  EIGEN_DEVICE_FUNC static void run(DstXprType &dst, const SrcXprType &src, const internal::assign_op<typename DstXprType::Scalar> &/*func*/)
  {
    eigen_assert(dst.rows() == src.rows() && dst.cols() == src.cols());
    src.evalTo(dst);
  }
};

} // namespace internal

} // end namespace Eigen

#endif // EIGEN_ASSIGN_EVALUATOR_H
