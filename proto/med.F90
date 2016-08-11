module MED

!-----------------------------------------------------------------------------
! Mediator Component.
!-----------------------------------------------------------------------------

  use ESMF
  use NUOPC
  use NUOPC_Mediator, only: &
    mediator_label_DataInitialize  => label_DataInitialize, &
    mediator_routine_SS             => SetServices, &
    mediator_routine_Run            => routine_Run, &
    mediator_label_Advance          => label_Advance, &
    mediator_label_CheckImport      => label_CheckImport, &
    mediator_label_SetRunClock      => label_SetRunClock, &
    NUOPC_MediatorGet
  use UTILS
  
  implicit none
  
  private
  
  public SetServices
  
!-----------------------------------------------------------------------------
  contains
!-----------------------------------------------------------------------------
  
  subroutine SetServices(mediator, rc)
    type(ESMF_GridComp)  :: mediator
    integer, intent(out) :: rc
    
    rc = ESMF_SUCCESS
    
    ! the NUOPC model component will register the generic methods
    call NUOPC_CompDerive(mediator, mediator_routine_SS, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    ! -> switching to IPD versions is done in InitializeP0
    call ESMF_GridCompSetEntryPoint(mediator, ESMF_METHOD_INITIALIZE, &
      userRoutine=InitializeP0, phase=0, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) return ! bail out

    ! set entry point for methods that require specific implementation
    call NUOPC_CompSetEntryPoint(mediator, ESMF_METHOD_INITIALIZE, &
      phaseLabelList=(/"IPDv03p1"/), userRoutine=InitializeAdvertise, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call NUOPC_CompSetEntryPoint(mediator, ESMF_METHOD_INITIALIZE, &
      phaseLabelList=(/"IPDv03p5"/), userRoutine=InitializeRealize, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    call NUOPC_CompSetEntryPoint(mediator, ESMF_METHOD_RUN, &
      phaseLabelList=(/"MedRunPhase"/), userRoutine=mediator_routine_Run, &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call NUOPC_CompSpecialize(mediator, specLabel=mediator_label_SetRunClock, &
      specPhaseLabel="MedRunPhase", specRoutine=SetRunClock, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call NUOPC_CompSpecialize(mediator, specLabel=mediator_label_CheckImport, &
      specPhaseLabel="MedRunPhase", specRoutine=CheckImport, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call NUOPC_CompSpecialize(mediator, specLabel=mediator_label_Advance, &
      specPhaseLabel="MedRunPhase", specRoutine=MediatorAdvance, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    call NUOPC_CompSpecialize(mediator, specLabel=mediator_label_DataInitialize, &
         specRoutine=DataInitialize, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, &
         file=__FILE__)) &
         return  ! bail out
    
   
  end subroutine SetServices
  
!-----------------------------------------------------------------------------
  subroutine CheckImport(mediator, rc)
!-----------------------------------------------------------------------------
! CheckImport is overridden to not check timestamps, as in ATM and OCN
    type(ESMF_GridComp)   :: mediator
    integer, intent(out)  :: rc

    ! This is the routine that enforces the explicit time dependence on the
    ! import fields. This simply means that the timestamps on the Fields in the
    ! importState are checked against the currentTime on the Component's 
    ! internalClock. Consequenty, this model starts out with forcing fields
    ! at the current time as it does its forward step from currentTime to 
    ! currentTime + timeStep.

    ! local variables
    type(ESMF_Clock)        :: clock
    type(ESMF_Time)         :: time
    type(ESMF_State)        :: importState
    logical                 :: allCurrent
    character(ESMF_MAXSTR)  :: name

    rc = ESMF_SUCCESS
    
    call ESMF_GridCompGet(mediator, name=name, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) return ! bail out

  end subroutine CheckImport
  
!-----------------------------------------------------------------------------
  subroutine SetRunClock(mediator, rc)
!-----------------------------------------------------------------------------
! SetRunClock forces the Mediator clock to always match that of the driver
! SetClock is the default, so the timeStep is the same as that of the driver
    type(ESMF_GridComp)   :: mediator
    integer, intent(out)  :: rc

    ! local variables
    type(ESMF_Clock)          :: mediatorClock, driverClock
    type(ESMF_Time)           :: currTime, stopTime
    type(ESMF_TimeInterval)   :: timeStep
    type(ESMF_Direction_Flag) :: direction

    rc = ESMF_SUCCESS

    call NUOPC_MediatorGet(mediator, mediatorClock=mediatorClock, &
      driverClock=driverClock, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) return ! bail out
   
    ! --- set the Clock to have the current start time as the driverClock
    call ESMF_ClockGet(driverClock, currTime=currTime, &
         timeStep=timeStep, direction=direction, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) return  ! bail out
    call ESMF_ClockSet(mediatorClock, currTime=currTime, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) return  ! bail out

  end subroutine SetRunClock

!-----------------------------------------------------------------------------
  subroutine InitializeP0(mediator, importState, exportState, clock, rc)
!-----------------------------------------------------------------------------
    type(ESMF_GridComp)   :: mediator
    type(ESMF_State)      :: importState, exportState
    type(ESMF_Clock)      :: clock
    integer, intent(out)  :: rc
    
    rc = ESMF_SUCCESS

    print *,' -- MED InitializeP0'
    ! Switch to IPDv03 by filtering all other phaseMap entries
    call NUOPC_CompFilterPhaseMap(mediator, ESMF_METHOD_INITIALIZE, &
      acceptStringList=(/"IPDv03p"/), rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
  end subroutine InitializeP0
  
!-----------------------------------------------------------------------------
  subroutine InitializeAdvertise(mediator, importState, exportState, clock, rc)
    type(ESMF_GridComp)  :: mediator
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc
    
    rc = ESMF_SUCCESS
    
    print *,' -- MED InitializeAdvertise'
    ! importable field: sea_surface_temperature
    call NUOPC_Advertise(importState, &
         StandardName="sea_surface_temperature", name="sst", &
         TransferOfferGeomObject='cannot provide', rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
      
    ! importable field: air_pressure_at_sea_level
    call NUOPC_Advertise(importState, &
         StandardName="air_pressure_at_sea_level", name="pmsl", &
         TransferOfferGeomObject='cannot provide', rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    ! importable field: surface_net_downward_shortwave_flux
    call NUOPC_Advertise(importState, &
         StandardName="surface_net_downward_shortwave_flux", name="rsns", &
         TransferOfferGeomObject='cannot provide', rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! exportable field: sea_surface_temperature
    call NUOPC_Advertise(exportState, &
         StandardName="sea_surface_temperature", name="sst", &
         TransferOfferGeomObject='cannot provide', rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! exportable field: air_pressure_at_sea_level
    call NUOPC_Advertise(exportState, &
         StandardName="air_pressure_at_sea_level", name="pmsl", &
         TransferOfferGeomObject='cannot provide', rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    ! exportable field: surface_net_downward_shortwave_flux
    call NUOPC_Advertise(exportState, &
         StandardName="surface_net_downward_shortwave_flux", name="rsns", &
         TransferOfferGeomObject='cannot provide', rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

  end subroutine InitializeAdvertise
  
!-----------------------------------------------------------------------------
  subroutine InitializeRealize(mediator, importState, exportState, clock, rc)
    type(ESMF_GridComp)  :: mediator
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc
    
    ! local variables    
    type(ESMF_Field)        :: field
    real(ESMF_KIND_R8), pointer :: ptr(:,:)
    
    rc = ESMF_SUCCESS   

    print *,' -- MED InitializeRealize'

    ! access the "sst" field in the importState
    call ESMF_StateGet(importState, field=field, itemName="sst", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    ! the transferred Grid is already set, allocate memory for data by complete
    call ESMF_FieldEmptyComplete(field, typekind=ESMF_TYPEKIND_R8, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_FieldGet(field, farrayPtr=ptr, rc=rc)
    ptr = -100000
    call minmax2d('MEDrealize sst imp:',ptr)
    
    ! access the "pmsl" field in the importState
    call ESMF_StateGet(importState, field=field, itemName="pmsl", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_FieldEmptyComplete(field, typekind=ESMF_TYPEKIND_R8, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_FieldGet(field, farrayPtr=ptr, rc=rc)
    ptr = -100000
    call minmax2d('MEDrealize pmsl imp:',ptr)
    
    ! access the "rsns" field in the importState
    call ESMF_StateGet(importState, field=field, itemName="rsns", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out    
    call ESMF_FieldEmptyComplete(field, typekind=ESMF_TYPEKIND_R8, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_FieldGet(field, farrayPtr=ptr, rc=rc)
    ptr = -100000
    call minmax2d('MEDrealize rsns imp:',ptr)
        
    ! access the "sst" field in the exportState
    call ESMF_StateGet(exportState, field=field, itemName="sst", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_FieldEmptyComplete(field, typekind=ESMF_TYPEKIND_R8, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_FieldGet(field, farrayPtr=ptr, rc=rc)
    ptr = -100000
    call minmax2d('MEDrealize sst exp:',ptr)
       
    ! access the "rsns" field in the exportState
    call ESMF_StateGet(exportState, field=field, itemName="pmsl", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_FieldEmptyComplete(field, typekind=ESMF_TYPEKIND_R8, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_FieldGet(field, farrayPtr=ptr, rc=rc)
    ptr = -100000
    call minmax2d('MEDrealize pmsl exp:',ptr)
   
    ! access the "pmsl" field in the exportState
    call ESMF_StateGet(exportState, field=field, itemName="rsns", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out    
    ! the transferred Grid is already set, allocate memory for data by complete
    call ESMF_FieldEmptyComplete(field, typekind=ESMF_TYPEKIND_R8, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_FieldGet(field, farrayPtr=ptr, rc=rc)
    ptr = -100000
    call minmax2d('MEDrealize rsns exp:',ptr)
    
    
  end subroutine InitializeRealize
  
!-----------------------------------------------------------------------------
  subroutine DataInitialize(mediator, rc)
!-----------------------------------------------------------------------------
! Resolve Data-Dependencies during Initialize
! Avoids this error:
! Initialize data-dependency resolution loop has entered a dead-lock situation.
    type(ESMF_GridComp)  :: mediator
    integer, intent(out) :: rc
    
    type(ESMF_Field)                            :: imField, exField
    real(ESMF_KIND_R8), pointer                 :: imFPtr(:,:), exFPtr(:,:)

    type(ESMF_State)                  :: importState
    type(ESMF_State)                  :: exportState

    rc = ESMF_SUCCESS
    
    print *,' -- MED DataInitialize'
    ! query the Component for its exportState
    call NUOPC_MediatorGet(mediator, importState=importState, &
         exportState=exportState, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    call ESMF_StateGet(importState, itemName="sst", field=imField, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) return ! bail out
    call ESMF_FieldGet(imField, farrayPtr=imFPtr, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) return ! bail out=
    call minmax2d('MEDdata sst imp:',imFPtr)
    
    call ESMF_StateGet(importState, itemName="pmsl", field=imField, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) return ! bail out
    call ESMF_FieldGet(imField, farrayPtr=imFPtr, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) return ! bail out=
    call minmax2d('MEDdata pmsl imp:',imFPtr)

    call ESMF_StateGet(importState, itemName="rsns", field=imField, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) return ! bail out
    call ESMF_FieldGet(imField, farrayPtr=imFPtr, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) return ! bail out=
    call minmax2d('MEDdata rsns imp:',imFPtr)

    call ESMF_StateGet(exportState, itemName="sst", field=exField, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) return ! bail out
    call ESMF_FieldGet(exField, farrayPtr=exFPtr, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) return ! bail out
    call minmax2d('MEDdata sst exp:',exFPtr)

    call ESMF_StateGet(exportState, itemName="pmsl", field=exField, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) return ! bail out
    call ESMF_FieldGet(exField, farrayPtr=exFPtr, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) return ! bail out
    call minmax2d('MEDdata pmsl exp:',exFPtr)

    call ESMF_StateGet(exportState, itemName="rsns", field=exField, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) return ! bail out
    call ESMF_FieldGet(exField, farrayPtr=exFPtr, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) return ! bail out
    call minmax2d('MEDdata rsns exp:',exFPtr)

    ! Once a component has set its InitializeDataComplete Attribute to "true"
    ! it, and the Connectors to it, will no longer be called during the remainder
    ! of the resolution loop.
    call NUOPC_CompAttributeSet(mediator, &
      name="InitializeDataComplete", value="true", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
  end subroutine DataInitialize
  
!-----------------------------------------------------------------------------
  subroutine MediatorAdvance(mediator, rc)
    type(ESMF_GridComp)  :: mediator
    integer, intent(out) :: rc
    
    ! local variables
    type(ESMF_Clock)              :: clock
    type(ESMF_State)              :: importState, exportState
    type(ESMF_Field)                            :: imField, exField
    real(ESMF_KIND_R8), pointer                 :: imFPtr(:,:), exFPtr(:,:)

    rc = ESMF_SUCCESS
    
    print *,' -- MED MediatorAdvance'
    
    call NUOPC_MediatorGet(mediator, mediatorClock=clock, &
         importState=importState, &
         exportState=exportState, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call fieldCopyImp2Exp(importState, exportState, 'sst', rc)
    call fieldCopyImp2Exp(importState, exportState, 'pmsl', rc)
    call fieldCopyImp2Exp(importState, exportState, 'rsns', rc)
!!$    call fieldCopyExp2Imp(exportState, importState, 'sst', rc=rc)
!!$    call fieldCopyExp2Imp(exportState, importState, 'pmsl', rc=rc)
!!$    call fieldCopyExp2Imp(exportState, importState, 'rsns', rc=rc)
    
    call ESMF_StateGet(importState, itemName="sst", field=imField, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) return ! bail out
    call ESMF_FieldGet(imField, farrayPtr=imFPtr, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) return ! bail out=
    call minmax2d('MEDadvance sst imp:',imFPtr)
    
    call ESMF_StateGet(importState, itemName="pmsl", field=imField, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) return ! bail out
    call ESMF_FieldGet(imField, farrayPtr=imFPtr, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) return ! bail out=
    call minmax2d('MEDadvance pmsl imp:',imFPtr)

    call ESMF_StateGet(importState, itemName="rsns", field=imField, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) return ! bail out
    call ESMF_FieldGet(imField, farrayPtr=imFPtr, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) return ! bail out=
    call minmax2d('MEDadvance rsns imp:',imFPtr)

    call ESMF_StateGet(exportState, itemName="sst", field=exField, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) return ! bail out
    call ESMF_FieldGet(exField, farrayPtr=exFPtr, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) return ! bail out
    call minmax2d('MEDadvance sst exp:',exFPtr)

    call ESMF_StateGet(exportState, itemName="pmsl", field=exField, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) return ! bail out
    call ESMF_FieldGet(exField, farrayPtr=exFPtr, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) return ! bail out
    call minmax2d('MEDadvance pmsl exp:',exFPtr)

    call ESMF_StateGet(exportState, itemName="rsns", field=exField, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) return ! bail out
    call ESMF_FieldGet(exField, farrayPtr=exFPtr, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) return ! bail out
    call minmax2d('MEDadvance rsns exp:',exFPtr)


    ! HERE THE MEDIATOR does the mediation of Fields that come in on the
    ! importState with a timestamp consistent to the currTime of the 
    ! mediators Clock.
    
    ! The Mediator uses the data on the import Fields to update the data
    ! held by Fields in the exportState.
    
    ! After this routine returns the generic Mediator will correctly
    ! timestamp the export Fields and update the Mediator Clock to:
    !
    !       currTime -> currTime + timeStep
    !
    ! Where the timeStep is equal to the parent timeStep.
    
    call ESMF_ClockPrint(clock, options="currTime", &
      preString="------>MED Advance from: ", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    call ESMF_ClockPrint(clock, options="stopTime", &
      preString="-------------------> to: ", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
     
  end subroutine MediatorAdvance

end module MED
