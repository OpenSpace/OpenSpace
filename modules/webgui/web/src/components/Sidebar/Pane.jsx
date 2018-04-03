import React from 'react';
import PropTypes from 'prop-types';
import Icon from '../common/Icon/Icon';

import styles from './Pane.scss';

const Pane = ({ children, title, closeCallback }) => (
  <section className={styles.Pane}>
    <header>
      <div className={styles.title}>
        { title }
      </div>

      { closeCallback && (
        <button onClick={closeCallback(false)} className={styles.close}>
          <Icon icon="close" className="small" />
        </button>
      ) }
    </header>
    <div className={styles.content}>
      { children }
    </div>
  </section>
);

Pane.propTypes = {
  children: PropTypes.node,
  closeCallback: PropTypes.func,
  title: PropTypes.node,
};

Pane.defaultProps = {
  children: [],
  closeCallback: null,
  title: null,
};

Pane.styles = styles;

export default Pane;
