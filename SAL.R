# Aktifkan semua library yang dibutuhkan
library(maps)
library(SpatialVx)
library(ncdf4)
library(sp)
library(spatstat)
library(raster)
library(Thermimage)


# Import file NetCDF dari Satelit GSMaP
nc_data <- nc_open('E:/skripsi/final/satelit.nc', readunlim=TRUE)
lon <- ncvar_get(nc_data,"lon")
nlon <- dim(lon)
head(lon); nlon
lat <- ncvar_get(nc_data,"lat")
nlat <- dim(lat)
head(lat); nlat
lonlat <- as.matrix(expand.grid(lon,lat))
time <- ncvar_get(nc_data,"time")
dim(time)
time_units <- ncatt_get(nc_data,"time","units")
time_units 
nt <- dim(time)
nt

# Ubah variable varid sesuai dengan variabel yang berada pada data NetCDF GSMaP
temp_array <- ncvar_get(nc = nc_data, varid="precip", verbose=T)

# Pilih waktu data yang akan diverifikasi, pada hal ini data yang akan diverifikasi adalah data pada waktu 00 UTC / ke-1
temp_total <- temp_array[,,1]
tmp_vec_long <- as.vector(temp_total)
length(tmp_vec_long)
temp_mat <- matrix(tmp_vec_long, nrow=nlon, ncol=nlat)

# Import file NetCDF hasil keluaran model WRF
nc_data2 <- nc_open('E:/skripsi/final/wrf.nc', readunlim=TRUE)
lon2 <- ncvar_get(nc_data2,"lon")
nlon2 <- dim(lon2)
head(lon2); nlon2
lat2 <- ncvar_get(nc_data2,"lat")
nlat2 <- dim(lat2)
head(lat2); nlat2
lonlat2 <- as.matrix(expand.grid(lon2,lat2))
time2 <- ncvar_get(nc_data2,"time")
dim(time2)
time_units2 <- ncatt_get(nc_data2,"time","units")
time_units2 
nt2 <- dim(time2)
nt2

# Ubah variable varid sesuai dengan variabel yang berada pada data NetCDF WRF
temp_array2 <- ncvar_get(nc = nc_data2, varid="rainc", verbose=T)+ncvar_get(nc = nc_data2, varid="rainsh", verbose=T)

# Pilih waktu data yang akan diverifikasi, pada hal ini data yang akan diverifikasi adalah data pada waktu 00 UTC
# Karena data curah hujan hasil keluaran WRF berupa hasil akumulasi maka nilai curah hujannya dikurangi curah hujan pada waktu sebelumnya
temp_subs <- temp_array2[,,25]-temp_array2[,,24]
tmp_vec_long2 <- as.vector(temp_subs)

# Karena perbedaan struktur pada data NetCDF WRF maka data harus di mirror terhadap sumbu y
temp_mat2 <- mirror.matrix(matrix(tmp_vec_long2, nrow=nlon2, ncol=nlat2))

# Mengecek dimensi data wrf dan obs
dim(temp_mat)
dim(temp_mat2)

#Menyamakan dimensi data wrf dengan satelit
temp_mat2_adjs <- temp_mat2[1:116,1:55]

# Verifikasi SAL

# temp_mat = data verifikator, temp_mat2 = data yang akan diverifikasi
loc <- as.matrix(lonlat)
x <- make.SpatialVx(temp_mat, temp_mat2_adjs,
                    loc = loc, 
                    projection = TRUE, 
                    map = TRUE, 
                    loc.byrow = FALSE, 
                    field.type = "Curah Hujan", units = "mm/jam",
                    data.name = "Precipitation Analysis", 
                    obs.name = "Observasi Satelit",model.name = "Model WRF" )
x
plot(x)

# Percentile
# Mendapatkan nilai persentil dari masing-masing domain
thr1 <- quantile(temp_mat,probs=0.95)
thr2 <- quantile(temp_mat2_adjs,probs=0.95)
look <- FeatureFinder(x,
                      do.smooth = FALSE, 
                      smoothpar = 5, 
                      thresh = c(thr1,thr2), 
                      fac=1/15)
look
plot(look)
saller(look)

# Absolute
look <- FeatureFinder(x,
                      do.smooth = FALSE, 
                      smoothpar = 5, 
                      thresh = 1)
look
plot(look)
saller(look)
