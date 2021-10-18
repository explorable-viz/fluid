FROM node

RUN wget -q -O - https://dl-ssl.google.com/linux/linux_signing_key.pub | apt-key add -
RUN echo "deb http://dl.google.com/linux/chrome/deb/ stable main" > /etc/apt/sources.list.d/google.list
RUN apt-get update
RUN apt-get install -y google-chrome-stable xvfb

RUN apt-get update && apt-get -y install sudo
RUN sudo apt-get install libtinfo5
RUN curl --compressed -o- -L https://yarnpkg.com/install.sh | bash
