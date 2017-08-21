import React from 'react';
import Row from '../../components/common/Row/Row';
import styles from './About.scss';
import logo from './logo.png';
import LoadingString from '../../components/common/LoadingString/LoadingString';

const About = () => (
  <Row className={styles.about}>
    <section>
      <img src={logo} alt="OpenSpace Logo" className={styles.img} />
    </section>
    <section>
      <h1>OpenSpace</h1>
      <p>
        <a href="http://openspaceproject.com" target="_blank" rel="noopener noreferrer">
          openspaceproject.com
        </a>
        &nbsp;Version <LoadingString loading>0.9.0 build aaaaaa</LoadingString>
      </p>
      <p>
        OpenSpace is open source interactive data
        visualization software designed to visualize
        the entire known universe and portray our
        ongoing efforts to investigate the cosmos.
      </p>
      <p>
        &copy; 2014 - { (new Date()).getUTCFullYear() }
        &nbsp;
        <a href="http://openspaceproject.com/?page_id=24" target="_blank" rel="noopener noreferrer">
          OpenSpace Development Team
        </a>
      </p>
    </section>
  </Row>
);

export default About;
