module UTILS

  use ESMF
  use NUOPC
  implicit none

  private

  public fieldCopyImp2Exp
  public fieldCopyExp2Imp
  public minmax2d

contains

!-----------------------------------------------------------------------------
  subroutine minmax2d(msg, ptr)
!-----------------------------------------------------------------------------
    character(len=*), intent(in)  :: msg
    real(ESMF_KIND_R8), pointer   :: Ptr(:,:)

    write(6,'(a24,1x,2(es15.5e3))')trim(msg),minval(Ptr),maxval(Ptr)

  end subroutine minmax2d

!-----------------------------------------------------------------------------
  subroutine fieldCopyImp2Exp(importState, exportState, fieldName, rc)
!-----------------------------------------------------------------------------
    type(ESMF_State), intent(inout)             :: importState
    type(ESMF_State), intent(inout)             :: exportState
    character(len=*), intent(in)                :: fieldName
    integer, optional, intent(out)              :: rc

    type(ESMF_Field)                            :: imField, exField
    real(ESMF_KIND_R8), pointer                 :: imFPtr(:,:), exFPtr(:,:)
    integer                                     :: i, j, clb(2), cub(2)
    type(ESMF_StateItem_Flag)                   :: itemType
    character(len=64) :: name
    character(len=64) :: impname
    character(len=64) :: expname
    character(len=256) :: mesg

    if(present(rc)) rc = ESMF_SUCCESS

    call ESMF_StateGet(importState, name=impname, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, &
         file=__FILE__)) &
         return  ! bail out

    call ESMF_StateGet(exportState, name=expname, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, &
         file=__FILE__)) &
         return  ! bail out

    call ESMF_StateGet(importState, itemName=fieldName, itemtype=itemType, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, &
         file=__FILE__)) &
         return  ! bail out
    print *,' -- UTILS copy ',trim(fieldname), &
         ' from ',trim(impname),' to ',trim(expname)

    ! If nothing to copy, return immediately
    if(itemType==ESMF_STATEITEM_NOTFOUND) return

    call ESMF_StateGet(exportState, itemName=fieldName, itemtype=itemType, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, &
         file=__FILE__)) &
         return  ! bail out

    ! If nothing to copy, return immediately
    if(itemType==ESMF_STATEITEM_NOTFOUND) return

    call ESMF_StateGet(importState, itemName=fieldName, field=imField, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, &
         file=__FILE__)) &
         return  ! bail out
    call ESMF_FieldGet(imField, farrayPtr=imFPtr, computationalLBound=clb, &
         computationalUBound=cub, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, &
         file=__FILE__)) &
         return  ! bail out

    call ESMF_StateGet(exportState, itemName=fieldName, field=exField, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, &
         file=__FILE__)) &
         return  ! bail out

    call ESMF_FieldGet(exField, farrayPtr=exFPtr, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, &
         file=__FILE__)) &
         return  ! bail out
    
    call minmax2d('UTILS imp:',imFPtr)

    do i = clb(1), cub(1)
      do j = clb(2), cub(2)
        exFPtr(i,j) = imFPtr(i,j)
      enddo
    enddo

    call minmax2d('UTILS exp:',exFPtr)

  end subroutine fieldCopyImp2Exp

!-----------------------------------------------------------------------------
  subroutine fieldCopyExp2Imp(exportState, importState, fieldName, rc)
!-----------------------------------------------------------------------------
    type(ESMF_State), intent(inout)             :: exportState
    type(ESMF_State), intent(inout)             :: importState
    character(len=*), intent(in)                :: fieldName
    integer, optional, intent(out)              :: rc

    type(ESMF_Field)                            :: imField, exField
    real(ESMF_KIND_R8), pointer                 :: imFPtr(:,:), exFPtr(:,:)
    integer                                     :: i, j, clb(2), cub(2)
    type(ESMF_StateItem_Flag)                   :: itemType
    character(len=64) :: name
    character(len=64) :: impname
    character(len=64) :: expname
    character(len=256) :: mesg

    if(present(rc)) rc = ESMF_SUCCESS

    call ESMF_StateGet(exportState, name=expname, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, &
         file=__FILE__)) &
         return  ! bail out

    call ESMF_StateGet(importState, name=impname, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, &
         file=__FILE__)) &
         return  ! bail out

    call ESMF_StateGet(exportState, itemName=fieldName, itemtype=itemType, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, &
         file=__FILE__)) &
         return  ! bail out

    print *,' -- UTILS copy ',trim(fieldname), &
         ' from ',trim(expname),' to ',trim(impname)
    
    ! If nothing to copy, return immediately
    if(itemType==ESMF_STATEITEM_NOTFOUND) return

    call ESMF_StateGet(importState, itemName=fieldName, itemtype=itemType, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, &
         file=__FILE__)) &
         return  ! bail out

    ! If nothing to copy, return immediately
    if(itemType==ESMF_STATEITEM_NOTFOUND) return

    call ESMF_StateGet(importState, itemName=fieldName, field=imField, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, &
         file=__FILE__)) &
         return  ! bail out

    call ESMF_StateGet(exportState, itemName=fieldName, field=exField, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, &
         file=__FILE__)) &
         return  ! bail out

    call ESMF_FieldGet(imField, farrayPtr=imFPtr, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, &
         file=__FILE__)) &
         return  ! bail out

    call ESMF_FieldGet(exField, farrayPtr=exFPtr, computationalLBound=clb, &
         computationalUBound=cub, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, &
         file=__FILE__)) &
         return  ! bail out

    call minmax2d('UTILS exp:',exFPtr)
    
    do i = clb(1), cub(1)
      do j = clb(2), cub(2)
        imFPtr(i,j) = exFPtr(i,j)
      enddo
    enddo

    call minmax2d('UTILS imp:',imFPtr)

  end subroutine fieldCopyExp2Imp
  
end module UTILS
