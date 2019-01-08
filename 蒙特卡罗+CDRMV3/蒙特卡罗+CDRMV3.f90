!***************************************************************************************
!***************************  2018年06月，长安大学 — 聂启阳   ***************************
!***************************************************************************************
    PROGRAM McCdrmv3
	Real res,sum,cum,max,ress,sums,cums,maxs
	Real ynavrg,sumyn,Maxnse,Deff1,Deff2,Max1,SCHongFeng      !实测洪峰与模拟洪峰
	Integer T,i,k,ns,nobasin,position,position1,SCHFt         !实测洪峰峰现时间
    Real(4) SCHongLiang                                       !实测洪量
	CHARACTER namebasin*2
    Integer, dimension (:), allocatable      :: MNHFt         !模拟洪峰峰现时间
    Real(4), dimension (:), allocatable      :: MNHongLiang   !模拟洪量
    Real, dimension (:), allocatable      :: Num,ObsIn,y,yn,MaxMCy,dumy,qin,qins,Rep,RepQ,MNHongFeng  !模拟洪峰
    Real, dimension (:), allocatable      :: dumy2,dumy3,dumy4,dumy5,dumy6,dumy7,dumy8,dumy9,dumy10,dumy11
	Real, dimension (:,:), allocatable	  :: MCy,e,eyn
    Real(4), dimension (:), allocatable   :: sls,rrmse,nse,abias,paixu

!	PARAMETER :
	Real Nwlb,Nwup,Nu1lb,Nu1up,Nu2lb,Nu2up,Nf2lb,Nf2up,NRvlb,NRvup,F1lb,F1up,ASOUlb,ASOUup,TOUSUIMSlb,TOUSUIMSup,Betaclb,Betacup	
    Real(4), dimension (:), allocatable   :: Nwmc,Nu1mc,Nu2mc,Nf2mc,NRvmc,F1mc,ASOUmc,TOUSUIMSmc,Betacmc
	real(4) ranval1,ranval2,ranval3,ranval4,ranval5,ranval6,ranval7,ranval8,ranval9,ranval10

	WRITE (*,*)'*************************蒙特卡罗校准模型*************************'
	WRITE (*,*)

!	蒙特卡洛计算数:	      
	ns		=4

!************************************************读取输入数据************************************************
    
	OPEN (111,FILE='./输出数据/MC/实测数据.txt')		     
	OPEN (11,FILE='./输出数据/MC/历次蒙特卡罗生成参数.txt')		 			  
	OPEN (333,FILE='./输出数据/MC/历次模拟结果与实测.csv')         
	OPEN (555,FILE='./输出数据/MC/历次检验参数.txt')				   !OF for Inflow
	OPEN (17,FILE='./输出数据/MC/最佳参数.asc')				   !流入的最佳参数
	OPEN (19,FILE='./输出数据/MC/符合要求参数.asc')			   !良好的流入和流出参数
    
    WRITE (17,'(13A14)') '模拟序号','纳什系数','洪峰相对误差','洪量相对误差','城市','森林','河道','降雨折算','土壤深','导水率','下渗','农地','荒地'
    WRITE (19,'(13A10)') '模拟序号','纳什效率系数','洪峰流量相对误差','洪量相对误差','城市','森林','河道','降雨折算','土壤深','导水率','下渗','农地','荒地'

    
    
    
	READ (111,*) T

    Allocate (Num(T),ObsIn(T),y(T),yn(T),MaxMCy(T+1))
    Allocate (dumy(T),qin(T),dumy2(T),dumy3(T),dumy4(T),dumy5(T),dumy6(T),dumy7(T),dumy8(T),dumy9(T),dumy10(T),dumy11(T))

	DO 5 i=1,T
		READ  (111,*) Num(i),ObsIn(i)
  5	ENDDO
	sumyn=0.0
    SCHongFeng=ObsIn(1)
		DO 10 i=1,T
			yn(i)=ObsIn(i)     !实测流量过程
			sumyn=sumyn+yn(i)
            if (SCHongFeng<=yn(i)) then
                SCHongFeng=yn(i)
                SCHFt=i            !冒泡法求出实测洪峰流量与峰现时间  nie
            end if           
    10  ENDDO

    SCHongLiang=3600.0*(sumyn-(yn(1)+yn(t))/2)
			ynavrg=sumyn/T                     !ObsIn 平均值

!	************************************************************************************************************
!   *******************蒙特卡洛实验：MC PARAMETER输入数据:均匀分布;下边界（lb）和上边界（up）*******************
!	************************************************************************************************************
!		Nu1=par1      !城市
!		Nf=par2       !森林
!		NRv=par3      !河道
!		F1=par4       !降雨除去植被截留与蒸发后的比例系数
!		ASOU=par5     !影响土壤深度
!		TOUSUIMS=par6 !导水率
!		Betac=par7
!        np=par8       !农地
!        nw=par9       !荒地

	Nwlb		=0.2542        
	Nwup		=0.5329        
    
	Nu1lb		=0.3763		
	Nu1up		=0.9043        
    
	Nu2lb		=0.001		
	Nu2up		=0.010       
    
	Nf2lb		=0.7		
	Nf2up		=1.3       
    
	NRvlb		=75.0	
	NRvup		=136.062     
    
	F1lb		=0.001		
	F1up		=0.0012       
    
	ASOUlb		=3.0
	ASOUup		=8.0      
    
	TOUSUIMSlb	=0.4548	
	TOUSUIMSup	=0.978  
    
	Betaclb		=0.01	
	Betacup		=0.09  
		


	Allocate (sls(ns),rrmse(ns),nse(ns),abias(ns),paixu(ns),qins(ns),Rep(ns),RepQ(ns),MNHongFeng(ns))
    Allocate (MNHFt(ns))
    Allocate (MNHongLiang(ns))
    Allocate (MCy(ns,T),e(ns,T),eyn(ns,T))
	Allocate (Nwmc(ns),Nu1mc(ns),Nu2mc(ns),Nf2mc(ns),NRvmc(ns),F1mc(ns),ASOUmc(ns),TOUSUIMSmc(ns),Betacmc(ns))


	call random_seed()
    write(333,*)'模拟数据',',','实测数据'
    WRITE(555,'(5A15)') '   k    ','    sls    ','    rrmse    ','    se    ','    abias    '
    
!	通过蒙特卡洛和目标函数运行仿真
		DO 20 k=1,ns
			res=0.0
			sum=0.0
			cum=0.0
			ress=0.0
			sums=0.0
			cums=0.0

			call random_number (ranval1) 
			call random_number (ranval2) 
			call random_number (ranval3) 
			call random_number (ranval4) 
			call random_number (ranval5) 
			call random_number (ranval6) 
			call random_number (ranval7)
			call random_number (ranval8) 
			call random_number (ranval9) 

!**********************生成在区间的内的随机数**********************
			Nwmc(k)=Nwlb+(ranval1*(Nwup-Nwlb))
			Nu1mc(k)=Nu1lb+(ranval2*(Nu1up-Nu1lb))
			Nu2mc(k)=Nu2lb+(ranval3*(Nu2up-Nu2lb))
			Nf2mc(k)=Nf2lb+(ranval4*(Nf2up-Nf2lb))
			NRvmc(k)=NRvlb+(ranval5*(NRvup-NRvlb))
			F1mc(k)=F1lb+(ranval6*(F1up-F1lb))
			ASOUmc(k)=ASOUlb+(ranval7*(ASOUup-ASOUlb))
			TOUSUIMSmc(k)=TOUSUIMSlb+(ranval8*(TOUSUIMSup-TOUSUIMSlb))
			Betacmc(k)=Betaclb+(ranval9*(Betacup-Betaclb))
!******************************************************************
            
			WRITE (11,'(I8,10F15.4)') K,Nwmc(k),Nu1mc(k),Nu2mc(k),Nf2mc(k),NRvmc(k),F1mc(k),ASOUmc(k),TOUSUIMSmc(k),Betacmc(k)   !*******写到mcparamaterall.txt 里*******

!			MCy(k,1)=0.0
			OPEN (12,FILE='./输出数据/MC/蒙特卡罗参数.txt',STATUS='OLD')                                  !输入单个参数  
            read(12,*)
			WRITE (12,'(9F15.5)') Nwmc(k),Nu1mc(k),Nu2mc(k),Nf2mc(k),NRvmc(k),F1mc(k),ASOUmc(k),TOUSUIMSmc(k),Betacmc(k)   !*******写到mcparamatera.txt里*******

			WRITE (*,'(A19,1x,I6,A3,I6,3x,A)') '迭代次数 :',k,'/',ns,' : XXX 河' 
			WRITE (*,'(5F12.4)') Nwmc(k),Nu1mc(k),Nu2mc(k),Nf2mc(k)       
			WRITE (*,'(5F12.4)') NRvmc(k),F1mc(k),ASOUmc(k),TOUSUIMSmc(k),Betacmc(k)                                        !**************参数写到屏幕里***********
			CLOSE(12)      
			
!			CALL system('CDRMv3.exe')
            
			OPEN (666, file='./输出数据/流域出流.dat')  ! 输出模型
			READ (666,*)
			
			WRITE (333,'(I14)') k        !在mcRunoff里写出第几次模拟
			
			if (T>0) then
            MNHongFeng(k)=0.0
            qins(k)=0.0
			DO 25 i=1,T
				READ (666,*) dumy(i),dumy2(i),dumy3(i),dumy4(i),dumy5(i),dumy6(i),qin(i),dumy7(i),dumy8(i), dumy9(i),dumy10(i),dumy11(i)
				MCy(k,i)=qin(i)            !流域输出流量
                qins(k)=qins(k)+qin(i)

!				2. The 输出数据s of MC simulation as the input for MATLAB Software（MC模拟的输出作为MATLAB软件的输入）
				WRITE (333,*) MCy(k,i),',',ObsIn(i)

!				
!				SLS :
					e(k,i)=yn(i)-MCy(k,i)   ! residuals
					res=res+(e(k,i)**2)
!				Abias :
					sum=sum+ABS(e(k,i)/(T))
!				NSE	  :
					eyn(k,i)=yn(i)-ynavrg
					cum=cum+(eyn(k,i)**2)
!				Peako :					
					MaxMCy(i)=MCy(k,i)
!					MaxMCy(5)=1000.0		! for checking
					IF (i==1) THEN
						MaxMCy(i)=MaxMCy(i)
					ELSEIF (MaxMCy(i)>=MaxMCy(i-1)) THEN
						MaxMCy(i)=MaxMCy(i)
					ELSE
						MaxMCy(i)=MaxMCy(i-1)
					ENDIF
					Max=MaxMCy(i)
!               模拟洪峰流量与峰现时间：
                    
            if (MNHongFeng(k)<=MCy(k,i)) then
                MNHongFeng(k)=MCy(k,i)       !冒泡法求出实测洪峰流量与峰现时间  nie
                MNHFt(k)=i
            end if           
				
25          ENDDO	
                
            MNHongLiang(k)=3600.0*(qins(k)-(MCy(k,1)+MCy(k,t))/2)   !模拟洪量
            Rep(k)=ABS(SCHongFeng-MNHongFeng(k))/SCHongFeng
            RepQ(k)=ABS(SCHongLiang-MNHongLiang(k))/SCHongLiang
			WRITE (333,'(I8)') 

!			Index Performance for Qinlet :
!				SLS (Simple Least Squares)
				sls(k)=res     
!				rrmse           
				rrmse(k)=(SQRT(res/T))/ynavrg   			!RMSE
!				NSE (Nash-sutcliffe efficiency) 
				nse(k)=1.-(res/cum)   ! 纳什系数NSE：1-（res/cum）  
!				nse(k)=res/cum
!				Abias (Absolute Error)
				abias(k)=sum

!			
				
			else
				sls(k)=0.0
				rrmse(k)=0.0
				nse(k)=0.0
				abias(k)=0.0
				
			endif
		
			CLOSE(666)
                paixu(k)=nse(k)  !  nie   源程序重排序nse导致nse数组与别的数组不对应
				WRITE(555,'(I8,4F15.5)') k,sls(k),rrmse(k),nse(k),abias(k)														!Objective Function Qinlet
																							
  20	ENDDO
				
!**************重新排序******************
				do j=1,ns-1
					max1=nse(j)
					position=j
						do k=j+1,ns
							if (nse(k)>max1) then
								max1=nse(k)		    !冒泡法求得纳什效率系数最大值							
								position=k
							endif
								
 						enddo
 								position=position
								
!						SWAP Process:
								Deff1=nse(j)
								Deff2=nse(position)
								paixu(j)=Deff2
								paixu(position)=Deff1
								Maxnse=paixu(1)	    !将纳什效率系数按照从大到小排序					
                 enddo
    
        open (520,FILE='./输出数据/MC/绘图.csv')	
        
		DO 30 k=1,ns

				if(nse(k)==Maxnse) then
					WRITE (17,'(I8,12F15.5)') K,nse(k),Rep(k),RepQ(k),Nwmc(k),Nu1mc(k),Nu2mc(k),Nf2mc(k),NRvmc(k),F1mc(k),ASOUmc(k),TOUSUIMSmc(k),Betacmc(k)				!最好的参数
                    WRITE (17,'(I8,2F15.5,2I8,2F15.5)') K,SCHongFeng,MNHongFeng(k),SCHFt,MNHFt(k),SCHongLiang,MNHongLiang(k)
                    write (520,*) k
                    DO i=1,T
				        write (520,*) MCy(k,i),',',ObsIn(i)
                    END DO
                end if
                if(nse(k)>=0.70) then 
					WRITE (19,'(I8,12F15.5)') K,nse(k),Rep(k),RepQ(k),Nwmc(k),Nu1mc(k),Nu2mc(k),Nf2mc(k),NRvmc(k),F1mc(k),ASOUmc(k),TOUSUIMSmc(k),Betacmc(k)				!符合要求的参数
                    WRITE (19,'(I8,2F15.5,2I8,2F15.5)') K,SCHongFeng,MNHongFeng(k),SCHFt,MNHFt(k),SCHongLiang,MNHongLiang(k)
                    write (520,*) k
                    DO i=1,T
				        write (520,*) MCy(k,i),',',ObsIn(i)
                    END DO
                endif
  30	ENDDO
        write(*,*)'**************************蒙特卡洛调参结束**************************'	
		CLOSE (111)
		CLOSE (333)
		CLOSE (555)
		CLOSE (11)     
		CLOSE (17)
		CLOSE (19)
        close(520)

	END