#! /bin/bash

INSTALL_PATH="/usr/local/lib/python3.5/site-packages"
SRC_PATH=$(pwd)

sudo apt-get update && sudo apt-get upgrade
sudo apt-get install build-essential cmake pkg-config \
                     libjpeg8-dev libtiff5-dev libjasper-dev libpng12-dev \
                     libavcodec-dev libavformat-dev libswscale-dev libv4l-dev \
                     libxvidcore-dev libx264-dev \
                     libgtk-3-dev \
                     libatlas-base-dev gfortran \
                     python3.5-dev

mkdir ~/opencv_install && cd ~/opencv_install

wget -O opencv.zip https://github.com/Itseez/opencv/archive/3.2.0.zip
unzip opencv.zip

wget -O opencv_contrib.zip https://github.com/Itseez/opencv_contrib/archive/3.2.0.zip
unzip opencv_contrib.zip

sudo pip3 install virtualenv virtualenvwrapper
source /usr/local/bin/virtualenvwrapper.sh
mkvirtualenv -p /usr/bin/python3 cv

pip install numpy

cd opencv-3.2.0
mkdir -p build

cmake -D CMAKE_BUILD_TYPE=RELEASE \
      -D CMAKE_INSTALL_PREFIX=/usr/local \
      -D INSTALL_PYTHON_EXAMPLES=ON \
      -D INSTALL_C_EXAMPLES=OFF \
      -D OPENCV_EXTRA_MODULES_PATH="${SRC_PATH}"/opencv_contrib-3.2.0/modules \
      -D PYTHON_EXECUTABLE=~/.virtualenvs/cv/bin/python \
      -D BUILD_EXAMPLES=ON ..

make -j4 || make clean && make
sudo make install

sudo find ${INSTALL_PATH} -type f -name 'cv2*.so' -exec mv {} cv2.so \;
sudo ln -sf "${INSTALL_PATH}/cv2.so" ~/.virtualenv/cv/lib/python3.5/site-packages/
