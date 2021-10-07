FROM node

WORKDIR /usr/src/app
COPY . /usr/src/app
# RUN npm install

# Is sudo needed?
RUN apt-get update && apt-get -y install sudo
RUN sudo apt-get install libtinfo5
RUN curl --compressed -o- -L https://yarnpkg.com/install.sh | bash
RUN yarn install && yarn run bundle-app

CMD /bin/bash
